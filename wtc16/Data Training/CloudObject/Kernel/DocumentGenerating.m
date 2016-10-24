(* Mathematica package *)
BeginPackage["CloudObject`"];

System`DocumentGenerator;
(*System`RunDocumentGenerator::usage = "RunDocumentGenerator[CloudObject] runs the indicated generator in its own kernel, invoking all \
apparatus for logging, notification, and distribution.";*)
(*System`EvaluateDocumentGenerator::usage = "EvaluateDocumentGenerator[CloudObject] evaluates the generator in the current session, \
without logging, notification, or distribution.";*)
System`DocumentGeneratorInformation::usage = "DocumentGeneratorInformation[CloudObject] returns information about a generator.
DocumentGeneratorInformation[CloudObject, property] returns the value of the specified property.";
System`DocumentGeneratorInformationData;
System`DocumentGenerators::usage = "DocumentGenerators[] returns a list of the user's document generators, as cloud objects.";

(* Option symbols *)
System`DocumentDescription;
System`GeneratedDocumentHistoryLength;
System`MailOptions;
System`OutputFormat;

System`GeneratedDocumentBinding;

Begin["`Private`"];

Needs["JLink`"];

DocumentGenerator::listing = "Unable to obtain DocumentGenerator listing.";
DocumentGenerator::filex = "Cannot overwrite existing cloud object `1`."; 
DocumentGenerator::nffil = "`` not found."; 
DocumentGenerator::badarg = "Bad value `` for argument ``."; 
DocumentGenerator::badform = "Unrecognized output format ``."; 
DocumentGenerator::tcrea = "Unable to create generator task.";
DocumentGenerator::crea = "Unable to create generator.";
DocumentGenerator::notrep = "Object `` not recognized as a document generator.";
DocumentGenerator::notask = "No task found for object ``.";
DocumentGenerator::argu = "Unrecognized document generator specification.";
DocumentGenerator::nostart = "Unable to start task for document generator ``.";
DocumentGenerator::norm = "Unable to remove DocumentGenerator.";
DocumentGenerator::nostop = "Unable to stop DocumentGenerator `1`.";
DocumentGenerator::nonext = "Unable to obtain next scheduled run time for DocumentGenerator `1`.";

Unprotect[DocumentGenerator, DocumentGenerators, DocumentGeneratorInformation, CloudObject];
(*
DocumentGenerator::sched = "`1` is not a recognized scheduling time specification.";
DocumentGenerator::nouri = "Unrecognized uri specificiation `1`.";
DocumentGenerator::noavil = "Scheduling tasks remotely is not yet available.";
DocumentGenerator::restr = "Use restricted under current subscription.";
DocumentGenerator::upda = "Unable to update scheduled task.";
*)

Unprotect[DocumentGenerator, CloudObject];
SetAttributes[DocumentGenerator, {ReadProtected}];

$docGenMimeTypes = {"application/vnd.wolfram.bundle.document-generator"};
(* Slow! *)
documentGeneratorQ[co_CloudObject] := MemberQ[$docGenMimeTypes, CloudObjectInformation[co, "MimeType"]];

Options[DocumentGenerator] = Sort@ Join[{
		DocumentDescription -> None,
		Epilog -> None,
	    GeneratedDocumentHistoryLength -> Infinity,
	    MailOptions -> {},
	    
	    OutputFormat -> "StaticPage",
	    Permissions -> Automatic (* Applies to generated documents *)
	},
	Options[ScheduledTask]
];

driverTypeQ[x_] := MatchQ[x, Alternatives @@ {
	None,
	_File, Delayed[_File],
	_CloudObject, Delayed[_CloudObject],
	_Association (*, Delayed[_Association] *),
	_Function
}];

templateTypeQ[x_] := MatchQ[x, Alternatives @@ {
	_File, Delayed[_File],
	_CloudObject, Delayed[_CloudObject],
	_TemplateObject (* Delayed[_TemplateObject] *)
}];

epilogTypeQ[x_] := MatchQ[x, Alternatives @@ {
	None,
	_File, Delayed[_File],
	_CloudObject, Delayed[_CloudObject],
	_Function
}];

(* No driver *)
DocumentGenerator /: CloudDeploy[DocumentGenerator[t_?templateTypeQ, sched_, o:OptionsPattern[]], args___] :=
	CloudDeploy[DocumentGenerator[t, None, sched, o], args];
	
(* Unnamed CO generator *)
DocumentGenerator /: CloudDeploy[r:DocumentGenerator[t_?templateTypeQ, d_?driverTypeQ, sched_, o:OptionsPattern[]],
	oD:OptionsPattern[]] :=
	CloudDeploy[r, CloudObject[], oD];

(* Uri-named generator *)
DocumentGenerator /: CloudDeploy[r:DocumentGenerator[t_?templateTypeQ, d_?driverTypeQ, sched_, o:OptionsPattern[]],
	uri_String, oD:OptionsPattern[]] :=
	CloudDeploy[r, CloudObject[uri], oD];

DocumentGenerator /: CloudDeploy[r:DocumentGenerator[t_?templateTypeQ, d_?driverTypeQ, sched_, o:OptionsPattern[]],
	co_CloudObject, oD:OptionsPattern[]] :=
	Catch[iCloudDeployDocumentGenerator[r, co, oD], $tag]

handleGeneratingResponse[response_] := Switch[response,
	{$Failed, "restr"}, Message[DocumentGenerator::restr]; $Failed,
	{$Failed, "argu"}, Message[DocumentGenerator::argu]; $Failed,
	{$Failed, "upda"}, Message[DocumentGenerator::upda]; $Failed,
	{$Failed, "tcrea"}, Message[DocumentGenerator::tcrea]; $Failed,
	{$Failed, "crea"}, Message[DocumentGenerator::crea]; $Failed,
	{$Failed, "norm"}, Message[DocumentGenerator::norm]; $Failed,
	_, $Failed
]

(* No way to evaluate desktop file at runtime: *)
validateDocGenArg[a:"template"|"driver"|"epilog", Delayed[f_File]] := If[Not[TrueQ[$CloudEvaluation]],
	Message[DocumentGenerator::badarg, f, a]; False,
	True
];
validateDocGenArg["driver"|"epilog", None] := True;
validateDocGenArg["template", _?templateTypeQ] := True;
validateDocGenArg["driver", _?driverTypeQ] := True;
validateDocGenArg["epilog", _?epilogTypeQ] := True;
validateDocGenArg[__] := False;

iCloudDeployDocumentGenerator[
	r:DocumentGenerator[template_?templateTypeQ, driver_?driverTypeQ, sched_, o:OptionsPattern[]],
	co:CloudObject[uri_String, ___],
	oD:OptionsPattern[]
] := Module[
	{cron, opts = Join[Flatten[{o}], Options[DocumentGenerator]], resolveResource, epilog},
	
	cron = If[sched =!= None, timespec2Cron[sched], sched];
	
	epilog = Epilog /. opts;
	(* Most validation on server, but a few things here *)
	If[And @@ # === False, Return[$Failed]] & @ MapThread[
		validateDocGenArg[#1, #2] &,
		{
			{"template", "driver", "epilog"},
			{template, driver, epilog}
		}
	];

	(* Upload local files from a desktop client, take care of as many cases as possible
	 * on the server.
	 *)
	resolveResource[res_] := Check[
		Replace[res, {
			(*File[f_] :> CloudObject`Private`deleteable@CopyFile[FindFile[f], CloudObject[]] /; Not[TrueQ[$CloudEvaluation]]*)
			File[f_] :> With[{ff = FindFile[f]},
				Switch[ToUpperCase@FileExtension[ff],
					"NB"|"CDF",
					(* TODO CloudExport is a hack while CopyFile is broken; it will not support wl scripts. *)
					CloudObject`Private`deleteable@CloudExport[Get[ff], "NB"],
					
					_,
					Message[DocumentGenerator::badarg, res]
				]] (*/; Not[TrueQ[$CloudEvaluation]] ... object needs to be copied into user space, even in cloud *),
			t:Except[_Delayed|_CloudObject|None] :> CloudPut[t, SaveDefinitions -> True]
		}],
		Return[$Failed]
	];
	
	(If[MatchQ[#, $Failed | {$Failed, _} | _CloudSystem`DocumentGenerating`CreateCloudDocumentGenerator],
		handleGeneratingResponse[#],
		(* else *)
		co
	]) & [
		With[{t = resolveResource@template, d = resolveResource@driver, e = resolveResource@epilog},
			If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
				CloudSystem`DocumentGenerating`CreateCloudDocumentGenerator[co, t, d, cron,
					Sequence @@ Join[
						{
							TimeZone -> (TimeZone /. opts /. Automatic :> $TimeZone),
							Epilog -> e
						},
						opts
					]
				]
			]
		]
	]
]

iCloudDeployDocumentGenerator[DocumentGenerator[args___, o:OptionsPattern[]], ___] := 
	(ArgumentCountQ[DocumentGenerator, Length[Hold[args]], 3, 3]; $Failed)
iCloudDeployDocumentGenerator[___] := $Failed


iCloudStopDocumentGenerator[co_CloudObject] := Module[
	{task = DocumentGeneratorInformation[co, "Task"], uuid},
	If[Head[task] === System`ScheduledTaskInformationData &&
		MatchQ[uuid = Lookup[task[[1]], "UUID"], _String],

		If[MatchQ[#, $Failed | _CloudSystem`Scheduling`StopScheduledCloudTask],
			Message[DocumentGenerator::nostop, co];
			$Failed,
			(* else *)
			co
		] & [
			If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
				CloudSystem`Scheduling`StopScheduledCloudTask[uuid]
			]
		],
		(* else *)
		Message[DocumentGenerator::notask, co];
		$Failed
	]
]

iCloudStopDocumentGenerator[co_, OptionsPattern[]] := (Message[DocumentGenerator::nostop, co]; $Failed)


iCloudResumeDocumentGenerator[co_CloudObject] := Module[
	{task = DocumentGeneratorInformation[co, "Task"], uuid},
	If[Head[task] === System`ScheduledTaskInformationData &&
		MatchQ[uuid = Lookup[task[[1]], "UUID"], _String],

		If[MatchQ[#, $Failed | _CloudSystem`Scheduling`ResumeScheduledCloudTask],
			Message[DocumentGenerator::nostart, co];
			$Failed,
			(* else *)
			co
		] & [
			If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
				CloudSystem`Scheduling`ResumeScheduledCloudTask[uuid]
			]
		],
		(* else *)
		Message[DocumentGenerator::notask, co];
		$Failed
	]
]

iCloudResumeDocumentGenerator[st_, OptionsPattern[]] := (Message[DocumentGenerator::nostart, co]; $Failed)


Options[RunDocumentGenerator] = {
	GeneratedDocumentBinding -> Automatic
};
Options[EvaluateDocumentGenerator] = Append[Options[RunDocumentGenerator], {
	"LoggingFunction" -> None
}];

(*
 * Equivalent to "Run now" in web interface.
 *)
CloudObject /: RunDocumentGenerator[r_CloudObject, o:OptionsPattern[]] := With[
	{res = Catch[iCloudRunDocumentGenerator[r, o], $tag]},
	res
]

iCloudRunDocumentGenerator[co:CloudObject[uri_String, ___], o:OptionsPattern[]] := With[
	{},
	If[MatchQ[#, $Failed | {$Failed, _} | _CloudSystem`DocumentGenerating`RunCloudDocumentGenerator],
		handleGeneratingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`DocumentGenerating`RunCloudDocumentGenerator[co, o]
		]
	]
]

iCloudRunDocumentGenerator[r_, OptionsPattern[]] := handleGeneratingResponse[$Failed]

(*
 * Lower-level, will bypass task notification etc.
 *)
CloudObject /: EvaluateDocumentGenerator[r_CloudObject, o:OptionsPattern[]] := With[
	{res = Catch[iCloudEvaluateDocumentGenerator[r, CloudObject[], o], $tag]},
	res
]
CloudObject /: EvaluateDocumentGenerator[r_CloudObject, uri_String, o:OptionsPattern[]] := With[
	{res = Catch[iCloudEvaluateDocumentGenerator[r, CloudObject[uri], o], $tag]},
	res
]
CloudObject /: EvaluateDocumentGenerator[r_CloudObject, dest_CloudObject, o:OptionsPattern[]] := With[
	{res = Catch[iCloudEvaluateDocumentGenerator[r, dest, o], $tag]},
	res
]

iCloudEvaluateDocumentGenerator[co_CloudObject, dest_CloudObject, o:OptionsPattern[]] := With[
	{},
	(If[MatchQ[#, $Failed | {$Failed, _} | _CloudSystem`DocumentGenerating`EvaluateCloudDocumentGenerator],
		handleGeneratingResponse[#],
		(* else *)
		#
	]) & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`DocumentGenerating`EvaluateCloudDocumentGenerator[co, dest, o]
		]
	]
]

iCloudEvaluateDocumentGenerator[co_, OptionsPattern[]] := handleGeneratingResponse[$Failed]

(*
 * 
 *)
iCloudRemoveDocumentGenerator[co_CloudObject] := With[
	{},
	If[MatchQ[#, $Failed | {$Failed, _} | _CloudSystem`DocumentGenerating`RemoveCloudDocumentGenerator],
		handleGeneratingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`DocumentGenerating`RemoveCloudDocumentGenerator[co]
		]
	]
]

iCloudRemoveDocumentGenerator[co_, OptionsPattern[]] := (Message[DocumentGenerator::norm]; $Failed)


(* DocumentGeneratorInformation *)

DocumentGeneratorInformation[obj_?documentGeneratorQ] := iCloudDocumentGeneratorInformation[obj];

DocumentGeneratorInformation::noprop = "`1` is not a property returned by DocumentGeneratorInformation.";

DocumentGeneratorInformation[obj_CloudObject, property_] :=
	iCloudDocumentGeneratorInformation[obj] /. {
		System`DocumentGeneratorInformationData[assoc_] :>
			If[KeyExistsQ[assoc, property],
				assoc[property],
				(* else *)
				Message[DocumentGeneratorInformation::noprop, property];
				$Failed
			],
		_ :> $Failed
	}

iCloudDocumentGeneratorInformation[obj_CloudObject] := Module[
	{raw, med, taskInfo},
	raw = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
		CloudSystem`DocumentGenerating`Private`DocumentGeneratorImpl[obj, "CloudObjects" -> True]
	];

	If[MatchQ[raw, $Failed | _CloudSystem`DocumentGenerating`Private`DocumentGeneratorImpl],
		Message[DocumentGenerator::listing]; 
		$Failed,
		(* else *)
		raw = List @@ raw;
		med = Association[
			"Name" -> Lookup[raw, "Name"],
			"Directory" -> Lookup[raw, "Directory"],
			"UUID" -> Lookup[raw, "UUID"],
			DocumentDescription -> Lookup[raw, DocumentDescription],
			"Template" -> Lookup[raw, "Template"],
			"Driver" -> Lookup[raw, "Driver"],
			Epilog -> Lookup[raw, Epilog],
		    "GeneratedDocumentHistory" -> Lookup[raw, "GeneratedDocumentHistory"],
		    GeneratedDocumentHistoryLength -> Lookup[raw, GeneratedDocumentHistoryLength],
		    (*"CurrentDocument" -> Lookup[raw, "CurrentDocument"],*)
		    OutputFormat -> Lookup[raw, OutputFormat],
			Permissions -> Lookup[raw, Permissions],
    		"CurrentOutput" -> Lookup[raw, "OutputResource"],
   		    MailOptions -> Lookup[raw, MailOptions],
    		"Log" -> Lookup[raw, "Log"],
			(*"LastRunDate" -> Lookup[raw, "LastRunDate"],*)
			"Active" -> Lookup[raw, "Active", True]
		];
		taskInfo = formatRawTaskInfo[Lookup[raw, "Task"]];
		If[Head[taskInfo] === System`ScheduledTaskInformationData,
			AppendTo[med, "Task" -> taskInfo]
		];
		System`DocumentGeneratorInformationData[med]
	]
];

DocumentGenerators[] := Module[
	{},
	iCloudDocumentGenerators[]
]

iCloudDocumentGenerators[] := Module[
	{raw, med},
	raw = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
		CloudSystem`DocumentGenerating`Private`DocumentGeneratorsImpl[]
	];

	If[MatchQ[raw, $Failed | _CloudSystem`DocumentGenerating`Private`DocumentGeneratorsImpl],
		Message[DocumentGenerator::listing]; $Failed,
		(* else *)
		(* This sorts by next run date, soonest first. *)
		med = Reverse @ SortBy[raw, With[{task = Lookup[Options[#], "Task"]},
			If[MatchQ[task, {__Rule}],
				Lookup[task, "NextTimestamp"],
				(* else *)
				None
			]
		] & ];
		CloudObject[JoinURL[$CloudBase, "objects", Lookup[Options[#], "UUID"]]] & /@ med
	]
]


iCloudNextDocumentGeneratorTime[co:CloudObject[uri_String, ___]] := Module[
	{task = DocumentGeneratorInformation[co, "Task"], uuid},
	If[Head[task] === System`ScheduledTaskInformationData &&
		MatchQ[uuid = Lookup[task[[1]], "UUID"], _String],

		If[MatchQ[#, $Failed | _CloudSystem`Scheduling`Private`scheduledJob],
			Message[DocumentGenerator::nonext, co];
			$Failed,
			(* else *)
			If[TrueQ@#[[5]],
				(* Active *)
				#[[4]] /. {Null -> None, d_?NumericQ :> DateObject[d]},
				(* else *)
				None
			]
		] & [
			If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
				CloudSystem`Scheduling`Private`scheduledJob[uuid]
			]
		],
		(* else *)
		Message[DocumentGenerator::notask, co];
		$Failed
	]
]

iCloudNextDocumentGeneratorTime[co_, OptionsPattern[]] := (Message[DocumentGenerator::nonext, co]; $Failed)

Protect[DocumentGenerator, DocumentGenerators, DocumentGeneratorInformation, CloudObject];

End[]

EndPackage[]
