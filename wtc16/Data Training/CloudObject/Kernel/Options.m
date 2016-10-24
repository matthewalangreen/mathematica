BeginPackage["CloudObject`"]

CloudObject::noicon = "No icon named `1` found for `2`.";
CloudObject::invmeta = "Invalid meta information `1`; a list of rules or Association with string keys expected."

Begin["`Private`"]

(* Options *)
Unprotect[CloudObject];

Off[Options::fnsym];

CloudObject /: Verbatim[Options][obj_CloudObject] :=
    {
        MetaInformation -> Options[obj, MetaInformation],
        IconRules -> Options[obj, IconRules],
        Permissions -> Options[obj, Permissions],
        "Active" -> Options[obj, "Active"]
    }

(*****************************************************************************)
(* MetaInformation *)

encodeMetaInformationValue[value_] := ToString[value, InputForm]
encodeMetaInformation[values:{(_String->_)...}] :=
    ExportString[values /. (key_ -> value_) :> (key -> encodeMetaInformationValue[value]), "JSON", "Compact"->True]
encodeMetaInformation[key_String -> value_] := encodeMetaInformation[{key -> value}]
encodeMetaInformation[other_] :=
    (Message[CloudObject::invmeta, other]; $Failed)

decodeMetaInformationValue[value_] := ToExpression[value]
decodeMetaInformation[values_] :=
    ImportString[values, "JSON"] /. (key_ -> value_) :> (key -> decodeMetaInformationValue[value])

deleteMetaInformation[obj_CloudObject, key_String] :=
    execute[obj, "DELETE", "files", {"properties", key}]

CloudObject /: SetOptions[obj_CloudObject, MetaInformation -> key_String -> value_] :=
    responseCheck[execute[obj, "PUT", "files", {"properties", key}, Body -> encodeMetaInformationValue[value]], SetOptions]

CloudObject /: SetOptions[obj_CloudObject, MetaInformation -> values_] :=
    Module[{encoded, existing},
        encoded = encodeMetaInformation[values];
        If[encoded =!= $Failed,
            (* Delete existing (non-hidden) options first. TODO: This is quite inefficient. *)
            existing = Options[obj, MetaInformation];
            If[existing =!= $Failed,
                deleteMetaInformation[obj, #]& /@ Keys[existing];
            ];
            (* Post new options (only if non-empty, since server throws error otherwise). *)
            If[Length[values] > 0,
                responseCheck[execute[obj, "POST", "files", {"properties"}, Body -> encoded], SetOptions]
            ]
        ]
    ]

CloudObject /: Verbatim[Options][obj_CloudObject, MetaInformation -> key_String] :=
    execute[obj, "GET", "files", {"properties", key}] /. {
        HTTPError[404, ___] :>
            If[FileExistsQ[obj],
                Missing["Undefined"],
                Message[CloudObject::cloudnf, obj];
                $Failed
            ],
        {type_String, contentBytes:{_Integer ...}} :>
        (* server is returning a JSON object for some reason *)
            decodeMetaInformationValue[ImportString[FromCharacterCode[contentBytes], "JSON"][[1, 2]]],
        _ :> (Message[CloudObject::srverr]; $Failed)
    }

CloudObject /: Verbatim[Options][obj_CloudObject, MetaInformation] :=
    Module[{content, rules},
        content = responseToString @ execute[obj, "GET", "files", {"properties"}];
        If[content === $Failed, Return[$Failed]];
        If[content === "", Return[{}]];
        rules = decodeMetaInformation[content];
        DeleteCases[rules,
            Rule[key_String /;
                StringMatchQ[key, StartOfString ~~ "__" ~~ ___ ~~ EndOfString],
                _]
        ]
    ]

(*****************************************************************************)
(* Permissions *)

CloudObject /: SetOptions[obj_CloudObject, Permissions -> permissions_] :=
    Module[{cloud, uuid, mimetype, accessJSON},
        mimetype = CloudObjectInformation[obj, "MimeType"];
        If[mimetype === $Failed, Return[$Failed]];
        accessJSON = toJSON@normalizePermissions[permissions, mimetype];
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];

        execute[cloud, "PUT", {"files", uuid, "permissions"},
            Body -> ToCharacterCode[accessJSON]] /. {
            HTTPError[404, ___] :> (
                Message[CloudObject::cloudnf, obj];
                Return[$Failed]
            ),
            {_String, contentBytes:{_Integer ...}} :>
                (Permissions -> permissions),
            other_ :> ($lastError = other;
            Message[CloudObject::srverr, obj];
            Return[$Failed]
            )
        }
    ]

CloudObject /: Verbatim[Options][obj_CloudObject, Permissions] :=
    Module[{cloud, uuid, permjson, serverPermissions},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];

        permjson = execute[cloud, "GET", {"files", uuid, "permissions"}] /. {
            HTTPError[404, ___] :> (
                Message[CloudObject::cloudnf, obj];
                Return[$Failed]
            ),
            {_String, contentBytes:{_Integer ...}} :>
                FromCharacterCode[contentBytes],
            other_ :> ($lastError = other;
            Message[CloudObject::srverr, obj];
            Return[$Failed]
            )
        };

        serverPermissions = ImportString[permjson, "JSON"];
        Map[convertFromServerPermissions, serverPermissions]
    ]

(*****************************************************************************)
(* IconRules *)

CloudObject /: SetOptions[obj_CloudObject, IconRules -> iconRules_] :=
    IconRules -> SetCloudIcons[obj, iconRules, Asynchronous->True,
        "DeleteUnmentionedIcons" -> True, (* needed only with SetOptions *)
        "Content" :> CloudGet[obj], (* RuleDelayed b/c Content will only be used for Automatic icons *)
        "Deployment" :> (* RuleDelayed b/c Deployment will only be used for Automatic icons *)
            With[{info = CloudObjectInformation[obj]},
                With[{type = info[[1, "MimeType"]],
                    permissions = Options[obj, Permissions]}, (* workaround for the fact that CloudObjectInformation doesn't return permissions in the right format right now *)
                    iconDeploymentType[type, permissions]
                ]
            ]
    ]

CloudObject /: Verbatim[Options][obj_CloudObject, IconRules -> name_] :=
    Module[{cloud, uuid, icon},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];

        icon = execute[cloud, "GET", {"files", uuid, "icon", name}] /. {
            HTTPError[404, ___] :> (
                Message[CloudObject::noicon, name, obj];
                Return[$Failed]
            ),
            {type_String, contentBytes:{_Integer ...}} :> (
                $lastCloudObjectIconType = type;
                constructIcon[contentBytes, type]
            ),
            other_ :> ($lastError = other;
            Message[CloudObject::srverr, obj];
            Return[$Failed]
            )
        };

        If[Head[icon] === Image || Head[icon] === Graphics,
            icon,
            Message[CloudObject::noicon, name, obj];
            $Failed
        ]
    ]

constructIcon[contentBytes_List, type_] :=
    With[{contentString = FromCharacterCode[contentBytes]},
        tryImport[contentString, {type, "image/png", "image/jpg", "image/gif"}]
    ]

tryImport[content_, {}] := $Failed

tryImport[content_, {fmt_, rest___}] :=
    Quiet[ImportString[content, fmt]] /. {
        result_Image :> result,
        result_Graphics :> result,
        _ :> tryImport[content, {rest}]
    }

CloudObject /: Verbatim[Options][obj_CloudObject, IconRules] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        Cases[
            Map[
                # -> (Quiet[Options[obj, IconRules -> #]] /. {
                    img_Image :> img,
                    _ :> None
                }) &,
                Check[listIcons[cloud, uuid],
                    {"FileBrowser", "IOS", "Android", "WebPage"}
                ]
            ],
            Verbatim[Rule][_, _Image]
        ]
    ]

(*****************************************************************************)
(* "Active" *)

CloudObject /: Verbatim[Options][obj_CloudObject, "Active"] :=
    Module[{cloud, uuid, response},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];

        response = execute[cloud, "GET", {"files", uuid, "active"}] /. {
            HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj];
                Return[$Failed]),
            {_String, contentBytes:{_Integer ...}} :>
                FromCharacterCode[contentBytes],
            other_ :> ($lastError = other;
            Message[CloudObject::srverr, obj];
            Return[$Failed]
            )
        };

        StringMatchQ[StringTrim[response], "true" | "yes" | "t",
        	IgnoreCase->True]
    ]

CloudObject /: SetOptions[obj_CloudObject, "Active" -> activeQ_?BooleanQ] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];

        execute[cloud, "PUT", {"files", uuid, "active"},
            Body -> activeQBody[activeQ]] /. {
            HTTPError[404, ___] :> (Message[CloudObject::cloudnf, obj]; $Failed),
            {_String, contentBytes:{_Integer ...}} :> {"Active" -> activeQ},
            other_ :> ($lastError = other;
	            Message[CloudObject::srverr, obj];
    	        $Failed
            )
        }
    ]

activeQBody[True] = ToCharacterCode["true"]
activeQBody[False] = ToCharacterCode["false"]


On[Options::fnsym];

Protect[CloudObject];

End[]

EndPackage[]
