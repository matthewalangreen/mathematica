BeginPackage["CloudObject`"]

System`CloudGet;
System`CloudPut;
System`CloudSave;

CloudObject`GetObjectMetadataFilename;

Begin["`Private`"]

Needs["Iconize`"]

Unprotect[CloudObject];

(* general read/write *)

cleanup[tempfilename_, expr_: Null] := (DeleteCloudObject[tempfilename, Asynchronous->True]; expr)

readObject[obj_CloudObject, head_Symbol : CloudObject] :=
    responseToFile @ execute[obj]

writeObject[obj_CloudObject, content_, mimetype_,
        permissions_ : Automatic, iconRules_ : None, iconExpr_ : Null, metaInformation_ : {},
        head_Symbol : CloudObject] :=
    Module[{result},
        result = responseToString @ execute[obj, Automatic,
            UseUUID -> False, Body -> content, Type -> mimetype,
            Parameters -> {
                "permissions" -> escapeAndNormalizePermissions[permissions, mimetype],
                If[Length[metaInformation] > 0, "properties" -> encodeMetaInformation[metaInformation], Unevaluated[Sequence[]]]
            }];
        If[result === $Failed, Return[$Failed]];
        If[iconRules =!= None,
            SetCloudIcons[obj, iconRules, Asynchronous->True, "Content" -> iconExpr,
                "Deployment" -> iconDeploymentType[mimetype, permissions]]
        ];
        obj
    ]

(*Put*)

Unprotect[CloudPut];

definitionsToString[defs_] := StringJoin @ Riffle[Flatten[List @@ Replace[Unevaluated @ defs,
    (HoldForm[symbol_] -> def_) :> (Replace[Unevaluated@def, {
        (Attributes -> attributes_) :>
            If[Length[attributes] > 0,
                ToString[Unevaluated[Attributes[symbol] = attributes], InputForm],
                {}
            ],
        (DefaultValues -> options_) :>
            ReplaceAll[options,
                (Verbatim[HoldPattern][lhs_] -> rhs_) :>
                    ToString[Unevaluated[lhs = rhs], InputForm]
            ],
        (Messages -> messages_) :>
            ReplaceAll[messages,
                (Verbatim[HoldPattern][messagename_] -> message_) :>
                    ToString[Unevaluated[messagename = message], InputForm]
            ],
        (name_ -> values_) :>
            ReplaceAll[Unevaluated@values, {
                (lhs_ -> rhs_) :> ToString[Unevaluated[lhs = rhs], InputForm],
                (Verbatim[HoldPattern][lhs_] :> rhs_) :> ToString[Unevaluated[lhs := rhs], InputForm],
                (lhs_ :> rhs_) :> ToString[Unevaluated[lhs := rhs], InputForm]
            }]
    }, {1}]), {1}
]], "\n\n"]

Options[CloudPut] = {SaveDefinitions->False, Permissions->Automatic, IconRules->Automatic, MetaInformation->{}};
Options[iCloudPut] = Options[CloudPut];

iCloudPut[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], mimetype_String, opts:OptionsPattern[]] :=
    Module[{content, tempfilename,
        iconRules = OptionValue[CloudPut, {opts, objopts}, IconRules],
        metaInformation = OptionValue[CloudPut, {opts, objopts}, MetaInformation],
        permissions = OptionValue[CloudPut, {opts, objopts}, Permissions]
    },
        If[TrueQ[OptionValue[SaveDefinitions]],
        (* save definitions *)
            content = exprToStringBytesWithSaveDefinitions[Unevaluated[expr]],
        (* do not save definitions *)
            tempfilename = CreateTemporary[];
            Put[Unevaluated[expr], tempfilename];
            content = BinaryReadList[tempfilename];
        ];
        writeObject[obj, content, mimetype, permissions, iconRules, Unevaluated[expr], metaInformation]
    ]

exprToStringBytesWithSaveDefinitions[expr_] :=
    Module[{defs, content, exprLine},
        (*defs = Language`ExtendedFullDefinition[expr];*)
        defs = With[{excl = OptionValue[Language`ExtendedFullDefinition, ExcludedContexts]},
        	Language`ExtendedFullDefinition[expr, ExcludedContexts -> Complement[excl, {"CloudObject"}]]
        ];
        content = definitionsToString[defs];
        exprLine = ToString[Unevaluated[expr], InputForm];
        content = content <> "\n\n" <> exprLine <> "\n";
        ToCharacterCode[content, "UTF-8"]
    ]

CloudPut[expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[Unevaluated[expr], obj, formatToMimeType["Expression"], opts]

CloudPut[expr_, uri_String, opts:OptionsPattern[]] :=
    CloudPut[Unevaluated[expr], CloudObject[uri], opts]

CloudPut[expr_, options : OptionsPattern[]] :=
    CloudPut[Unevaluated[expr], CloudObject[], options]

CloudPut[args___] := (ArgumentCountQ[CloudPut,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Put[expr_, obj_CloudObject] := CloudPut[Unevaluated[expr], obj]

SetAttributes[CloudPut, {ReadProtected}];
Protect[CloudPut];

(*Save*)

Unprotect[CloudSave];

Options[CloudSave] = {IconRules->Automatic, MetaInformation->{}, Permissions->Automatic};
Attributes[CloudSave] = {HoldFirst};

CloudSave[expr_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], opts:OptionsPattern[]] :=
    Module[{type, content, tempfilename},
        If[FileExistsQ[obj],
            {tempfilename, type} = readObject[obj];
            If[tempfilename === $Failed, Return[$Failed]],
        (* else *)
            tempfilename = CreateTemporary[]
        ];
        Save[tempfilename, Unevaluated[expr]];
        content = BinaryReadList[tempfilename];
        writeObject[obj, content, formatToMimeType["Expression"],
            OptionValue[CloudSave, {opts, objopts}, Permissions],
            OptionValue[CloudSave, {opts, objopts}, IconRules], Unevaluated[expr],
            OptionValue[CloudSave, {opts, objopts}, MetaInformation],
            CloudSave
        ]
    ]

CloudSave[expr_, uri_String, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[uri], opts]

CloudSave[expr_, opts:OptionsPattern[]] := CloudSave[expr, CloudObject[], opts]

CloudSave[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudObject /: Save[obj_CloudObject, expr_] := CloudSave[expr, obj]

SetAttributes[CloudSave, {ReadProtected}];
Protect[CloudSave];

(*Get*)

Unprotect[CloudGet];

bundleMimeTypeQ[mimetype_] :=
    StringQ[mimetype] &&
        StringMatchQ[mimetype, "application/vnd.wolfram.bundle" ~~ ___]

CloudGet[co_CloudObject] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = readObject[co];
        Which[
            tempfilename === $Failed, $Failed,

            mimetype === "inode/directory", Message[Get::noopen, co]; $Failed,

            bundleMimeTypeQ[mimetype], CloudGet[FileNameJoin[{co, ".bundle"}]],

            True, cleanup[tempfilename, Block[{$CharacterEncoding = "UTF-8"},
                Get[tempfilename]
            ]]
        ]
    ];

CloudGet[uri_String] := CloudGet[CloudObject[uri]]

CloudObject /: Get[co_CloudObject] := CloudGet[co]

CloudGet[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

SetAttributes[CloudGet,{ReadProtected}];
Protect[CloudGet];

Protect[CloudObject];

(* From Jan, plus a tiny amount of error checking, and also allow CloudObject or UUID. *)
GetObjectMetadataFilename[obj_CloudObject, subpath___String] :=
    GetObjectMetadataFilename[getUUID[obj], subpath];

GetObjectMetadataFilename[uuid_String?UUIDQ, subpath___String] :=
    (* TODO (Jan?): does this need to use the $HomeDirectory of the CloudObject owner, not the caller?
       Or is that tautological? *)
    FileNameJoin[{$HomeDirectory, ".Objects", "metadata", StringTake[uuid, 3], uuid, subpath}];

GetObjectMetadataFilename[___] := $Failed;

End[]

EndPackage[]
