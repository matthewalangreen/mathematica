BeginPackage["CloudObject`"]

System`$Permissions;
System`Permissions;

General::invperm = "Invalid permissions specification `1`.";

Begin["`Private`"]

$Permissions = "Private";

permissionSpecs = "Read" | "Write" | "Execute" | "Edit" | "Save" | "EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" | "IncrementalEvaluate" | "Interact";

normalizePermissionsSpec["r", type_] = {"Read"};
normalizePermissionsSpec["w", type_] = {"Write"};
normalizePermissionsSpec["x", type_] = {"Execute"};

normalizePermissionsSpec["Edit", type_] := {"CellEdit", "CellCreate", "CellDelete"};

normalizePermissionsSpec[list_List, type_] :=
    Map[normalizePermissionsSpec[#, type]&, list]
normalizePermissionsSpec[spec_String?(StringMatchQ[#, Characters["rwx"]..]&), type_] :=
    Map[normalizePermissionsSpec[#, type]&, Characters[spec]]

normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression"] = {"Read"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.api"] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.computation"] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.fci"] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.form"] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/mathematica"] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook"] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook.element"] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, _] = {"Read"};

normalizePermissionsSpec[spec:permissionSpecs, type_] = {spec};

normalizePermissionsSpec[spec_, type_] := (Message[CloudObject::invperm, spec]; {})

normalizeUserSpec[user_String] :=
    If[StringFreeQ[user, ":"] && FreeQ[{"All", "Authenticated", "Owner"}, user],
        "user:" <> user,
        user
    ]

normalizePermissions["Public", type_] := {"All" -> Flatten[normalizePermissionsSpec[Automatic, type]], "Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions["Private", type_] := {"Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions[list_List, type_] := Join @@ Map[normalizePermissions[#, type]&, list]
normalizePermissions[user_String -> spec_, type_] := {normalizeUserSpec[user] -> Flatten[normalizePermissionsSpec[spec, type]]}
normalizePermissions[All -> spec_, type_] := normalizePermissions["All" -> spec, type]
normalizePermissions[spec_String, type_] := normalizePermissions[{"All" -> spec}, type]
normalizePermissions[Automatic, type_] := normalizePermissions[$Permissions, type]
normalizePermissions[spec_, type_] := (Message[CloudObject::invperm, spec]; {})
normalizePermissions[users_List -> spec_, type_] := Join @@ Map[normalizePermissions[# -> spec, type]&, users]

groupIdentifier[group_PermissionsGroup] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
        (* TODO: What should happen when the group is in a different cloud? *)
        If[uuid === None, Return[$Failed]];
        "wolfram:" <> uuid
    ]
normalizePermissions[group_PermissionsGroup -> spec_, type_] :=
    Module[{id},
        id = groupIdentifier[group];
        If[id === $Failed, (Message[CloudObject::invperm, group -> spec]; Return[{}])];
        normalizePermissions[id -> spec, type]
    ]

escapeAndNormalizePermissions = Composition[toJSON, normalizePermissions]

fromServerPermissions["r"] := "Read"
fromServerPermissions["w"] := "Write"
fromServerPermissions["x"] := "Execute"
fromServerPermissions[p:("Read" | "Write" | "Execute" | "Edit" | "Save" |
	"EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" |
	"IncrementalEvaluate" | "Interact")] := p

fromServerPermissions[permjson_] :=
    ImportString[permjson, "JSON"] /. {
        serverPermissions_List :>
            Map[convertFromServerPermissions, serverPermissions],
        other_ :> ($lastServerPermissionsJSON = permjson; $Failed)
    }

fromServerUserClass[class_] :=
    If[StringMatchQ[class, "wolfram:" ~~ __],
        PermissionsGroup[class], (* TODO: denormalize to the group's name, take into account the cloud base *)
        StringReplace[class, StartOfString ~~ "user:" -> ""]
    ]

convertFromServerPermissions[class_ -> perms_String] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, Characters[perms]], _String]
convertFromServerPermissions[class_ -> perms_List] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, perms], _String]

End[]

EndPackage[]
