BeginPackage["CloudObject`"]

System`CloudObjectInformation;
System`CloudObjectInformationData;
System`CloudObjects;

CloudObjects::invtype = "Invalid cloud object type specification `1`."

CloudObjectInformation::noprop = "`1` is not a property returned by CloudObjectInformation.";

Begin["`Private`"]

(* CloudObjects *)

Options[CloudObjects] = {"Directory" -> Automatic, "Type" -> All}

queryTypeValue[type_String] := formatToMimeType[type]
queryTypeValue[symbol_Symbol?headDeployFormatQ] := headMimeType[symbol]
queryTypeValue[Expression] := "application/vnd.wolfram.expression"
queryTypeValue[Notebook] := "application/mathematica"
queryTypeValue[ExternalBundle] := "application/vnd.wolfram.bundle"

queryTypeValue[All] = All;
queryTypeValue[Verbatim[Alternatives][types___]] := {types}
queryTypeValue[_] = $Failed;

iCloudObjects[cloud_String, path_, opts:OptionsPattern[CloudObjects]] :=
    Module[{query = {}, typevalue, type, obj, uuid},
        typevalue = OptionValue[CloudObjects, {opts}, "Type"];
        type = queryTypeValue[typevalue];
        If[type === $Failed,
            Message[CloudObjects::invtype, typevalue];
            type = All;
        ];
        If[MatchQ[type, _List],
            log["CloudObjects for multiple types `1`", type, DebugLevel->3];
            Return[Sort[Flatten[iCloudObjects[cloud, path, "Type"->#, opts]& /@ type]]];
        ];
        (* It would be as simple as the following, but the server does not actually support path=dir/* yet. *)
        (*If[type =!= All, AppendTo[query, "type" -> type]];
        responseToString[execute[cloud, "GET", {"files"}, Parameters->query], CloudObjects] /. (
            result_String :>
                uuidListingToCloudObjects[result]
        )*)
        If[type =!= All || path === All || path === "",
            log["CloudObjects: type=`1`, path=`2`", type, path, DebugLevel->3];
            If[type =!= All, AppendTo[query, "type" -> type]];
            If[path =!= All, AppendTo[query, "path" -> path]];
            responseToString[execute[cloud, "GET", {"files"}, Parameters->query], CloudObjects] /. (
                result_String :>
                    uuidListingToCloudObjects[result]
            ),
        (* No type is given. Use a different API which is actually working on the server. *)
            log["CloudObjects: path=`1`", path, DebugLevel->3];
            obj = CloudObject[cloud <> "/objects/" <> If[StringMatchQ[path, ___ ~~ "/*"], StringDrop[path, -2], path]];
            log["Directory object: `1`", obj, DebugLevel->3];
            uuid = getCloudAndUUID[obj][[2]];
            log["Directory UUID: `1`", uuid, DebugLevel->3];
            If[!UUIDQ[uuid], Message[CloudObject::cloudnf, dir]; Return[$Failed]];
            responseToString[execute[cloud, "GET", {"files", uuid}], CloudObjects] /. {
                result_String :>
                    uuidListingToCloudObjects[result]
            }
        ]
    ]

CloudObjects[All, opts:OptionsPattern[]] := iCloudObjects[$CloudBase, All, opts]
CloudObjects[None, opts:OptionsPattern[]] := iCloudObjects[$CloudBase, "", opts]
CloudObjects[obj_CloudObject, opts:OptionsPattern[]] :=
    Module[{cloud, path, name},
        {cloud, path} = getCloudAndPathList[obj];
        name = StringJoin[Riffle[Join[path, {"*"}], "/"]];
        iCloudObjects[cloud, name, opts]
    ]

CloudObjects[Automatic, opts:OptionsPattern[]] := CloudObjects[CloudDirectory[], opts]
CloudObjects[dir_String, opts:OptionsPattern[]] := CloudObjects[CloudObject[dir], opts]

(* If no directory is given as positional argument, take the option value (with default Automatic). *)
CloudObjects[opts:OptionsPattern[]] := CloudObjects[OptionValue["Directory"], opts]

(* Expand an Association out into a list of rules, which get treated as options. *)
CloudObjects[before___, assoc_Association, after___] := CloudObjects[before, Normal[assoc], after]

CloudObjects[dir_, type:(_String|_Symbol), opts:OptionsPattern[]] := CloudObjects[dir, "Type" -> type, opts]

(* List objects *)
CloudObjectsByType[contentType_String] :=
	Module[{response, uuids},
		response = responseToString @ execute[$CloudBase, "GET", {"files"},
			Parameters->{"type" -> contentType}]; (* TODO support multiple mime types, e.g. to handle notebooks *)
		If[!StringQ[response], Return[$Failed]];
		uuids = Map[FileNameTake[#, -1]&, StringSplit[response]];
		Map[cloudObjectFromUUID, uuids]
	]

unnamedCloudObjects[] :=
	execute[$CloudBase, "GET", {"files"}, Parameters -> {"path" -> ""}] /. {
		err_HTTPError :> ($Failed), (* TODO what message should this issue? *)
		{_, bytes_List} :>
			uuidListingToCloudObjects[bytes]
	}

uuidListingToCloudObjects[bytes_List] :=
	uuidListingToCloudObjects[FromCharacterCode[bytes]]

uuidListingToCloudObjects[listing_String] :=
	Sort[Cases[Map[uuidListEntryToCloudObject, StringSplit[listing]], _CloudObject]]

uuidListEntryToCloudObject[text_String] :=
	StringDrop[text, 7] /. {
		uuid_?UUIDQ :> cloudObjectFromUUID[uuid],
		_ :> $Failed
	}

(* CloudObjectInformation *)

CloudObjectInformation[obj_CloudObject] := cloudObjectInformation[obj]

CloudObjectInformation[obj_CloudObject, property_String] :=
	cloudObjectInformation[obj] /. {
		CloudObjectInformationData[assoc_] :>
			If[KeyExistsQ[assoc, property],
				assoc[property],
				Message[CloudObjectInformation::noprop, property];
				$Failed
			],
		_ :> $Failed
	}

cloudObjectInformation[obj_CloudObject, msghd_:CloudObjectInformation] :=
	Module[{cloud, uuid, json, allinfo, files, mimetype, info, infoData},
		{cloud, uuid} = getCloudAndUUID[obj];
		If[!(StringQ[cloud] && UUIDQ[uuid]), Return[$Failed]];
		json = execute[cloud, "GET", {"files", uuid, "info"}] /. {
			HTTPError[404, ___] :> (Message[msghd::cloudnf, obj]; Return[$Failed]),
			{_String, content_List} :>
				($lastInfoJSON = FromCharacterCode[content]),
			other_ :> (Message[CloudObjectInformation::srverr]; Return[$Failed])
		};

		allinfo = ImportString[json, "JSON"];
		If[!ListQ[allinfo],
			Message[CloudObjectInformation::srverr];
			Return[$Failed]
		];

		files = Lookup[allinfo,
			If[KeyExistsQ[allinfo, "files"], "files", "directoryListing"]];
		If[files === {},
			Message[CloudObjectInformation::srverr]; (* internal error -- info about directories is broken *)
			Return[$Failed]
		];
		info = files[[1]];
		mimetype = Lookup[info, "type"];

		infoData = Association[{
			"UUID" -> Lookup[info, "uuid"],
			"Name" -> Lookup[info, "name"],
			"OwnerWolframUUID" -> Lookup[info,
				If[KeyExistsQ[info, "ownerUUID"], "ownerUUID", "owner"]],
			"MimeType" -> mimetype,
			"FileType" ->
				If[mimetype === "inode/directory" || bundleMimeTypeQ[mimetype],
					Directory,
					File
				],
			"FileByteCount" -> FromDigits[("fileSize" /. info)],
			"Created" -> DateObject[Lookup[info, "created"]],
			"LastAccessed" -> DateObject[Lookup[info, "lastAccessed"]],
			"LastModified" -> DateObject[Lookup[info, "lastModified"]],
			"Permissions" ->
				fromServerPermissions[Lookup[info, "filePermission"]]
		}];

		If[KeyExistsQ[info, "active"],
			AppendTo[infoData, "Active" -> Lookup[info, "active"]]
		];

		System`CloudObjectInformationData[infoData]
	]

End[]

EndPackage[]
