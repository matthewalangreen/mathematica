(* Mathematica package *)

Needs["Security`"]

Block[{$Path},
	Quiet[CloudObject[]]  (*ensure auto-loader doesn't fire during initialization*)
];

With[{CloudObject`Private`dir=DirectoryName[$InputFileName]}, (*change context so this doesn't show in Global`*)
	Get[FileNameJoin[{CloudObject`Private`dir, "UUID.m"}]];
	Get[FileNameJoin[{CloudObject`Private`dir, "URL.m"}]];
	Get[FileNameJoin[{CloudObject`Private`dir, "Dialogs.m"}]];
	Get[FileNameJoin[{CloudObject`Private`dir, "Authentication.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "CloudBase.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "CloudDirectory.m"}]];
	Get[FileNameJoin[{CloudObject`Private`dir, "CloudObjects.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "Delayed.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "JSON.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "ServerAPI.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "PermissionsGroups.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "Permissions.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "Icons.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "FileOperations.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "LoggingData.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "GetPutSave.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "ImportExport.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "CloudDeploy.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "ListInformation.m"}]];
	Get[FileNameJoin[{CloudObject`Private`dir, "URLOperations.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "CloudEvaluation.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "Options.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "EvaluationData.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "CloudSymbol.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "Scheduling.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "DocumentGenerating.m"}]];
    Get[FileNameJoin[{CloudObject`Private`dir, "AccountData.m"}]];
]

Remove[CloudObject`Private`dir];(*remove from Symbol Table*)
