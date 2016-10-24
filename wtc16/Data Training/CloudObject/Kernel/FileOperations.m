BeginPackage["CloudObject`"]

RenameFile::cldnm = "Cloud object `1` does not contain a name.";

Begin["`Private`"]

Unprotect[CloudObject];

(* CopyFile *)

CloudObject /: CopyFile[src_, obj:CloudObject[uri_, opts:OptionsPattern[CloudObject]]] := Module[{format, mimetype, content},
    format = FileFormat[src];
    mimetype = formatToMimeType[format];
    content = BinaryReadList[src];
    writeObject[obj, content, mimetype,
        OptionValue[CloudObject, {opts}, Permissions],
        OptionValue[CloudObject, {opts}, IconRules], Null,
        OptionValue[CloudObject, {opts}, MetaInformation],
        CopyFile
    ]
]

CloudObject /: CopyFile[obj_CloudObject, target_] := Module[{tempfilename, mimetype},
    {tempfilename, mimetype} = readObject[obj];
    If[tempfilename === $Failed, Return[$Failed]];
    cleanup[tempfilename, CopyFile[tempfilename, target]]
]

CloudObject /: CopyFile[src_CloudObject, target:CloudObject[uri_, opts:OptionsPattern[CloudObject]]] :=
    Module[{tempfilename, mimetype, content},
        {tempfilename, mimetype} = readObject[src];
        If[tempfilename === $Failed, Return[$Failed]];
        content = BinaryReadList[tempfilename];
        cleanup[tempfilename, writeObject[target, content, mimetype,
            OptionValue[CloudObject, {opts}, Permissions],
            OptionValue[CloudObject, {opts}, IconRules], Null,
            OptionValue[CloudObject, {opts}, MetaInformation],
            CopyFile
        ]]
    ]

(* RenameFile *)

Unprotect[RenameFile];

CloudObject /: RenameFile[src_CloudObject, dest_CloudObject] :=
    Module[{uuid, cloud, path},
        {cloud, uuid} = Quiet[getCloudAndUUID[src]];
        If[cloud === $Failed || uuid === $Failed,
            Message[RenameFile::cloudnf, src];
            Return[$Failed]
        ];
        {cloud, path} = getCloudAndPath[dest];
        If[path === $Failed,
            Message[RenameFile::cldnm, dest];
            Return[$Failed]
        ];
        execute[cloud, "PUT", {"files", uuid, "path"},
            Parameters -> {"path" -> path}] /. {
            {_String, _List} :> dest,
            HTTPError[400, ___] :> (Message[RenameFile::filex, dest]; $Failed),
            err_HTTPError :> (checkError[err, RenameFile]; $Failed),
            _ :> (Message[RenameFile::srverr]; $Failed)
        }
    ]

Protect[RenameFile];

(* RenameDirectory *)

Unprotect[RenameDirectory];

CloudObject /: RenameDirectory[src_CloudObject, dest_CloudObject] :=
    Module[{uuid, cloud, path},
        {cloud, uuid} = getCloudAndUUID[src];
        If[cloud === $Failed || uuid === $Failed,
            Message[RenameDirectory::cloudnf, src];
            Return[$Failed]
        ];
        {cloud, path} = getCloudAndPath[dest];
        If[path === $Failed,
            Message[RenameDirectory::cldnm, dest];
            Return[$Failed]
        ];
        execute[cloud, "PUT", {"files", uuid, "path"},
            Parameters -> {"path" -> path}] /. {
            {_String, _List} :> dest,
            err_HTTPError :> (checkError[err, RenameDirectory]; $Failed),
            _ :> (Message[RenameDirectory::srverr]; $Failed)
        }
    ]

getCloudAndPath[CloudObject[uri_String, ___]] := getCloudAndPath[uri]

getCloudAndPath[uri_String] :=
    parseURI[uri] /. {
        {cloud_, _, user_, path_List, ___} :>
            {cloud, user<>"/"<>FileNameJoin[path, OperatingSystem -> "Unix"]},
        {cloud_, uuid_String, _, None, _, extrapath_List, ___} :>
            {cloud,
                uuid<>"/"<>FileNameJoin[extrapath, OperatingSystem -> "Unix"]},
        _ :> {$Failed, $Failed}
    }

Protect[RenameDirectory];

(* ReadList *)

CloudObject /: ReadList[co_CloudObject, rest___] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = readObject[co];
        If[tempfilename === $Failed, Return[$Failed]];
        cleanup[tempfilename,
            Block[{$CharacterEncoding = "UTF-8"},
                ReadList[tempfilename, rest]
            ]
        ]
    ]

(* DeleteFile *)

Options[DeleteCloudObject] = {Asynchronous -> False};

DeleteCloudObject[co_CloudObject, OptionsPattern[]] :=
    responseCheck[execute[co, "DELETE", Asynchronous -> OptionValue[Asynchronous]], DeleteCloudObject]

CloudObject /: DeleteFile[f_CloudObject] := Module[
	{cloud, uuid, params},
    {cloud, uuid} = Quiet[getCloudAndUUID[f]];

    If[!UUIDQ[uuid], (* file not found *)
        Message[DeleteFile::nffil, f];
        Return[$Failed];
    ];

    params = {"filter" -> "file"};
    execute[cloud, "DELETE", {"files", uuid}, Parameters -> params] /. {
        {_String, _List} :> Return[Null] (* success *),
        HTTPError[404, ___] :> (Message[DeleteFile::nffil, f]; Return[$Failed]),
        HTTPError[412, ___] :> (Message[DeleteFile::fdir, f]; Return[$Failed]), (* attempted to delete directory *)
        other_ :> (Message[CloudObject::srverr]; Return[$Failed])
    };
];

Unprotect[DeleteFile];

DeleteFile[list : {___, _CloudObject, ___}] := (DeleteFile /@ list;)

Protect[DeleteFile];

(* DeleteDirectory *)

CloudObject /: DeleteDirectory[dir_CloudObject, OptionsPattern[]] :=
    Module[{recursive = TrueQ[OptionValue[DeleteContents]],
        cloud, uuid, params},
        {cloud, uuid} = Quiet[getCloudAndUUID[dir]];
        If[!UUIDQ[uuid], (* named directory not found *)
            Message[DeleteDirectory::nodir, dir];
            Return[$Failed];
        ];

        params = {"recursive" -> ToLowerCase@ToString[recursive], "filter" -> "directory"};
        execute[cloud, "DELETE", {"files", uuid}, Parameters -> params] /. {
            {_String, _List} :> Return[Null] (* success *),
            HTTPError[400, ___] :> If[recursive, Message[CloudObject::srverr]; $Failed, Null],
            HTTPError[404, ___] :> (Message[DeleteDirectory::nodir, dir]; Return[$Failed]),
            HTTPError[412, ___] :> (Message[DeleteDirectory::nodir, dir]; Return[$Failed]), (* attempted to delete file *)
            other_ :> (Message[CloudObject::srverr]; Return[$Failed])
        };
        (* assert: delete failed with status 400 and recursive was not true ->
			directory was not empty. *)
        (* if directory is a bundle type, does it have only the bundle file? *)
        (* TODO handle bundle type *)
        Message[DeleteDirectory::dirne, dir];
        $Failed
    ];

(* CreateDirectory *)

CloudObject /: CreateDirectory[co_CloudObject] :=
    responseCheck[
        execute[co, Automatic, UseUUID -> False, Type -> "inode/directory"] /. {
            HTTPError[400, ___] :> (Message[CreateDirectory::filex, co];
            Return[co])
        },
        CreateDirectory,
        co]

(* FileExistsQ *)

CloudObject /: FileExistsQ[CloudObject[uri_String, ___]] :=
    parseURI[uri] /. {
    (* parts: {cloud, uuid, user, path, ext, extraPath, search} *)

    (* anonymous cloud object *)
        {cloud_, uuid_String?UUIDQ, _, None, _, {}, _} :>
            MatchQ[execute[cloud, "GET", {"files", uuid, {"path"}}],
                {_String, _List}],
    (* named cloud object *)
        {cloud_, None, userid_, path_, _, _, _} :>
            MatchQ[execute[cloud, "GET", {"files"},
                Parameters -> {"path" -> userid <> "/" <>
                    FileNameJoin[path, OperatingSystem -> "Unix"]}],
                {_String, _List}],
    (* named cloud object inside unnamed directory *)
        {cloud_, diruuid_, userid_, None, _, extraPath_List, _} :>
            MatchQ[execute[cloud, "GET", {"files"},
                Parameters -> {"path" -> diruuid<>"/"<>
                    FileNameJoin[extraPath, OperatingSystem -> "Unix"]}],
                {_String, _List}]
    }

(* Cloud file name manipulation *)

Unprotect[FileNameJoin]

FileNameJoin[{CloudObject[uri_, rest___], path___}] := CloudObject[JoinURL[uri, path], rest]

Protect[FileNameJoin]

(* FileType *)

CloudObject /: FileType[co_CloudObject] :=
    With[{info = Quiet[cloudObjectInformation[co, FileType]]},
        If[Head[info] === System`CloudObjectInformationData,
            First[info]["FileType"],
            None (* FileType is quiet about non-existent files *)
        ]
    ]

(* DirectoryQ *)

CloudObject /: DirectoryQ[co_CloudObject] := FileType[co] === Directory

(* FileByteCount *)

CloudObject /: FileByteCount[co_CloudObject] :=
    cloudObjectInformation[co, FileByteCount] /. {
        System`CloudObjectInformationData[info_Association] :> info["FileByteCount"],
        _ :> $Failed
    }

Protect[CloudObject];

End[]

EndPackage[]
