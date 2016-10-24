(* Mathematica package *)
BeginPackage["CloudObject`"];

System`ScheduledTask;
System`EvaluateScheduledTask;
System`ScheduledTaskInformation::usage = "ScheduledTaskInformation[CloudObject] returns information about a task.
ScheduledTaskInformation[CloudObject, property] returns the value of the specified property.";
System`ScheduledTaskInformationData;

(* Option symbols *)
System`NotificationOptions;
System`IncludeDocumentGeneratorTasks;
System`AutoRemove;

Begin["`Private`"];

Needs["JLink`"];

ScheduledTask::notask = "Argument 1 in CloudDeploy is not a regonized ScheduledTask specification.";
ScheduledTask::nostart = "Unable to start ScheduledTask.";
ScheduledTask::nostop = "Unable to stop ScheduledTask `1`.";
ScheduledTask::sched = "`1` is not a recognized scheduling time specification.";
ScheduledTask::nouri = "Unrecognized uri specificiation `1`.";
ScheduledTask::noavil = "Scheduling tasks remotely is not yet available.";
ScheduledTask::norm = "Unable to remove ScheduledTask `1`.";
ScheduledTask::listing = "Unable to obtain ScheduledTask listing.";
ScheduledTask::nonext = "Unable to obtain next scheduled run time for ScheduledTask `1`.";
ScheduledTask::restr = "Use restricted under current subscription.";
ScheduledTask::argu = "Unrecognized scheduling specification.";
ScheduledTask::upda = "Unable to update scheduled task.";
ScheduledTask::crea = "Unable to create scheduled task.";

Unprotect[ScheduledTask, CloudObject];
SetAttributes[ScheduledTask, {HoldFirst, ReadProtected}];

Options[ScheduledTask] = Sort@{
	NotificationOptions :> {{$WolframID} -> All},
	TimeZone -> Automatic,
	AutoRemove -> False
};

ScheduledTask /: CloudDeploy[task_ScheduledTask, o:OptionsPattern[]] := 
	CloudDeploy[task, CloudObject[], o];

ScheduledTask /: CloudDeploy[task_ScheduledTask, uri_String, o:OptionsPattern[]] := 
	CloudDeploy[task, CloudObject[uri], o];

ScheduledTask /: CloudDeploy[task_ScheduledTask, co_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudDeployScheduledTask[task, co], $tag]}, 
	res
]


schedulingMetaInformation[ScheduledTask[task_, schedule_, ___], uri_, uuid_] := Module[
	{json, sched = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`Private`standardizeSchedule[timespec2Cron[schedule]]
		]
	},
	json = {
		"name" -> uri,
    	"uuid" -> uuid,
    	"cronSchedule" -> First[sched[[2]]],
    	"repeatCount" -> ToString[Last[sched]],
    	"userID" -> ToString[$WolframUUID],
    	"taskData" -> ToString[Unevaluated[task], InputForm],
    	"taskType" -> "script"
	};
	ExportString[json, "JSON", "Compact" -> True]
]
schedulingMetaInformation[___] := ""

handleSchedulingResponse[response_] := Switch[response,
	{$Failed,"restr"}, Message[ScheduledTask::restr];$Failed,
	{$Failed,"argu"}, Message[ScheduledTask::argu];$Failed,
	{$Failed,"upda"}, Message[ScheduledTask::upda];$Failed,
	{$Failed,"crea"}, Message[ScheduledTask::crea];$Failed,
	_, Message[ScheduledTask::nostart];$Failed
]

(*iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_]] := iCloudDeployScheduledTask[st, CloudObject[]]*)

iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_, o:OptionsPattern[]], obj:CloudObject[uri_String, ___], OptionsPattern[]] := Module[
	{schedule = timespec2Cron[sched], name = GetNameFromURI[uri],
		fun = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate], uuid,
		opts = Join[Flatten[{o}], Options[ScheduledTask]]
	},

	If[SameQ[schedule, $Failed],
		Message[ScheduledTask::sched, sched];
		Throw[$Failed, $tag]
	];
	Check[
		iCloudPut[st, obj, "application/vnd.wolfram.expression.task", SaveDefinitions -> True],
		Throw[$Failed,$tag]
	];
	(* Don't try to get the uuid until the object has been Put; if it's a named cloud object, it won't
	 * have a uuid until now. DocumentGenerators handle both types on the server.
	 *)
	Check[
		uuid = GetUUID[obj],
		Throw[$Failed, $tag]
	];

	Check[
		SetOptions[obj, MetaInformation -> "__Scheduling" -> schedulingMetaInformation[st, name, uuid]],
		Throw[$Failed, $tag]
	];
	
	With[{res = fun[
			CloudSystem`Scheduling`StartScheduledCloudTask[
				CloudSystem`Scheduling`CreateScheduledCloudTask[EvaluateScheduledTask[obj], name, schedule, uuid,
					Sequence @@ Join[
						{
							TimeZone -> (TimeZone /. opts /. Automatic :> $TimeZone)
						},
						opts
					]
				]
			]
		]},
		
		If[SameQ[res, True],
			obj,
			(* else *)
			handleSchedulingResponse[res];
			(* DeleteFile[obj]? *)
			Throw[$Failed, $tag]
		]
	]
]


iCloudDeployScheduledTask[ScheduledTask[args___],___] := (ArgumentCountQ[ScheduledTask,Length[Hold[args]],2,2];$Failed)
iCloudDeployScheduledTask[___] := $Failed

toSchedule[n_String] := Module[{cron},
    cron = StringSplit[n];
    If[Length@cron < 3 || Length@cron > 7, Return[$Failed]];
    cron = cron /. {
        (*{s_, m_, h_, dom_, m_, dow_, y_}:>{s, m, h, dom, m, dow, y},
        {s_, m_, h_, dom_, m_, dow_}:>{s, m, h, dom, m, dow, "*"},
        {h_, dom_, m_, dow_, y_}:>{"*", "*", h, dom, m, dow, y},
        {h_, dom_, m_, dow_}:>{"*", "*", h, dom, m, dow, "*"}*)
        
        
        {s_, m_, h_, dom_, mo_, dow_, y_}:>{s, m, h, dom, mo, dow, y}, (* quartz expression *)
        {m_, h_, dom_, mo_, dow_, y_}:>{"*", m, h, dom, mo, dow, y}, (* classic cron with optional year *)
        {m_, h_, dom_, mo_, dow_}:>{"*", m, h, dom, mo, dow, "*"}, (* classic cron *)
        {h_, dom_, mo_, dow_}:>{"*", "*", h, dom, mo, dow, "*"}
        
        
    };
    StringJoin[Riffle[ToUpperCase@cron, " "]]
]

toSchedule[___] := $Failed

current[spec_] := DateString[DateList[], spec]
(* We can remove this and instead use dowToCron *)
currentDOW[] := With[{date = DateList[]},(*TODO: figure out if these are actually accurate*)
  Which[
   DayMatchQ[date, Sunday], "1",
   DayMatchQ[date, Monday], "2",
   DayMatchQ[date, Tuesday], "3",
   DayMatchQ[date, Wednesday], "4",
   DayMatchQ[date, Thursday], "5",
   DayMatchQ[date, Friday], "6",
   DayMatchQ[date, Saturday], "7",
   True, "*"
   ]]

$TimeSpecs = {
   "Hourly" :> "* * * * ? *",
   "Daily" :> StringJoin["* ", current[{"HourShort"}], " * * ? *"],
   "Weekly" :> StringJoin["* ", current[{"HourShort"}], " ? * ", currentDOW[], " *"],
   "Monthly" :> StringJoin["* ", current[{"HourShort", " ", "DayShort"}], " * ? *"],
   "Yearly" :> StringJoin["* ", current[{"HourShort", " ", "DayShort", " ", "MonthShort"}], " ? *"]
   };

(*validateTimeSpec[{n_Integer?Positive, di:(_Integer|_DirectedInfinity)}] := {n, di}
validateTimeSpec[__] := $Failed
validateCronSpec[{cron_, di_DirectedInfinity}] := toSchedule[cron]*)

$AvailableTimeSpecs = First /@ $TimeSpecs;

resolveSecs = {
    {a_?NumberQ, "Second"} :> Round[a],
    {a_?NumberQ, "Minute"} :> Round[a*60],
    {a_?NumberQ, "Hour"} :> Round[a*3600],
    {a_?NumberQ, "Day"} :> Round[a*3600*24],
    {a_?NumberQ, "Week"} :> Round[a*3600*24*7],
    {a_?NumberQ, "Month"} :> Round[a*3600*24*30], (* 30 days *)
    {a_?NumberQ, "Quarter"} :> Round[a*3600*24*90], (* 90 days *)
    {a_?NumberQ, "Year"} :> Round[a*3600*24*365] (* 365 days *)
};

(* CRON Output *)
timespec2Cron[string_String] /;  MemberQ[$AvailableTimeSpecs, string] := {string /. $TimeSpecs, Infinity}
timespec2Cron[d_DateObject] := With[{spec=StringJoin["* ", System`Utilities`DateObjectToCronSpecification[d]]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[cron_String] := With[{spec=toSchedule[cron]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[{spec_String, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[spec];
    tmp[[2]] = di;
    tmp]
    
(* List[Integer, DirectedInfinity] Output *)
timespec2Cron[HoldPattern[q_Quantity]]:= If[CompatibleUnitQ[q, "Seconds"] ,{QuantityMagnitude[UnitConvert[q, "Seconds"]] /. resolveSecs, Infinity},$Failed] 
timespec2Cron[n_Integer?Positive] := {n,Infinity}
timespec2Cron[{spec_}] := timespec2Cron[{spec, 1}]
timespec2Cron[{n_Integer?Positive, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[n];
    tmp[[2]] = di;
    tmp]

(* ambiguous *)
timespec2Cron[{spec : Except[_List], count_Integer}] := With[{s = timespec2Cron[spec]}, {First[s], count} /; s =!= $Failed]
timespec2Cron[{start_DateObject, timespec_, end_DateObject}] := {start, timespec2Cron[timespec], end}
timespec2Cron[{start_DateObject, timespec_}] := {start, timespec2Cron[timespec]}
timespec2Cron[{timespec_, end_DateObject}] := {timespec2Cron[timespec], end}

(* None/dummy *)
timespec2Cron[dummy:None|Null] := {Null, 1};

(* failures *)
timespec2Cron[_DateObject, _DateObject] := Message[ScheduledTask::sched, "2 argument timespec is ambiguous. Please use a different form."]
timespec2Cron[__] := $Failed


CloudObject /: StopScheduledTask[co_CloudObject, o:OptionsPattern[]] /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudStopDocumentGenerator[co, o], $tag]},
	res
]

CloudObject /: StopScheduledTask[task_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudStopScheduledTask[task], $tag]},
	res
]

iCloudStopScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	(*If[MatchQ[#, $Failed | _CloudSystem`Scheduling`StopScheduledCloudTask],
		Message[ScheduledTask::nostop, co];
		$Failed,
		(* else *)
		co
	] & [*)
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`StopScheduledCloudTask[uuid]
		]

]

iCloudStopScheduledTask[st_,OptionsPattern[]] := (Message[ScheduledTask::nostop,st];$Failed)


CloudObject /: StartScheduledTask[co_CloudObject, o:OptionsPattern[]]  /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudResumeDocumentGenerator[co, o], $tag]},
	res
]

CloudObject /: StartScheduledTask[task_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudResumeScheduledTask[task], $tag]}, res
]

iCloudResumeScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`ResumeScheduledCloudTask],
		handleSchedulingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`ResumeScheduledCloudTask[uuid]
		]
	]
]

iCloudResumeScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]

(*
 * Equivalent to "Run now" in web interface.
 *)
CloudObject /: RunScheduledTask[co_CloudObject, o:OptionsPattern[]] := Module[
	{res = Catch[iCloudRunDocumentGenerator[co, o], $tag]},
	res
] /; documentGeneratorQ[co]

CloudObject /: RunScheduledTask[task_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudRunScheduledTask[task], $tag]},
	res
]

iCloudRunScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`ExecuteScheduledCloudTask],
		handleSchedulingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`ExecuteScheduledCloudTask[uuid]
		]
	]
]

iCloudRunScheduledTask[st_,OptionsPattern[]] := handleSchedulingResponse[$Failed]



CloudObject /: RemoveScheduledTask[co_CloudObject, o:OptionsPattern[]] /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudRemoveDocumentGenerator[co, o], $tag]},
	res
]

CloudObject /: RemoveScheduledTask[task_CloudObject] := With[
	{res = Catch[iCloudRemoveScheduledTask[task], $tag]},
	res
]

iCloudRemoveScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`RemoveScheduledCloudTask], Message[ScheduledTask::norm, co]; $Failed, co] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`RemoveScheduledCloudTask[uuid]
		]
	]
]

iCloudRemoveScheduledTask[st_, OptionsPattern[]] := (Message[ScheduledTask::norm, st]; $Failed)

(*not documented yet; gets ScheduledTask out of CloudObject and runs it*)
Unprotect[EvaluateScheduledTask];

CloudObject /: EvaluateScheduledTask[co_CloudObject, o:OptionsPattern[]] /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudEvaluateDocumentGenerator[co, CloudObject[], o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, uri_String, o:OptionsPattern[]]  /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudEvaluateDocumentGenerator[co, CloudObject[uri], o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, dest_CloudObject, o:OptionsPattern[]]  /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudEvaluateDocumentGenerator[co, dest, o], $tag]},
	res
]

CloudObject /: EvaluateScheduledTask[task_CloudObject] := With[
	{res = Catch[iCloudEvaluateScheduledTask[task],$tag]}, res]

iCloudEvaluateScheduledTask[co:CloudObject[uri_String, ___]]:= With[
	{task=Quiet[If[MatchQ[#,_ScheduledTask],#,$Failed]&[CloudGet[co]]]},
	If[UnsameQ[task,$Failed],First[task],handleSchedulingResponse[task]]
]

iCloudEvaluateScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]
(*
 * Hybrid task listing
 *)
Unprotect[ScheduledTasks];
Options[ScheduledTasks] = Options[iCloudScheduledTasks] = {
	IncludeDocumentGeneratorTasks -> False
};
$cloudScheduledTasksFlag = True;
ScheduledTasks[o:OptionsPattern[]] /; TrueQ[And[$CloudConnected, $cloudScheduledTasksFlag]] := Block[
	{$cloudScheduledTasksFlag = False},
	Join[ScheduledTasks[], iCloudScheduledTasks[o]]
]

iCloudScheduledTasks[o:OptionsPattern[]] := Module[
	{raw, med},
	raw = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
		CloudSystem`Scheduling`ScheduledCloudTasks[]
	];

	If[MatchQ[raw, $Failed | _CloudSystem`Scheduling`ScheduledCloudTasks],
		Message[ScheduledTask::listing]; {$Failed},
		(* else *)
		If[Not@TrueQ[OptionValue[IncludeDocumentGeneratorTasks]],
			raw = Select[raw, ("TaskType" /. Options[#]) =!= "document-generator" &]
		];
		(* This sorts by next run date, soonest first. *)
		med = Reverse @ SortBy[raw, #[[4]] &];
		CloudObject[JoinURL[$CloudBase, "objects", First[#]]] & /@ med
	]
]

(* ScheduledTaskInformation *)
Unprotect[ScheduledTaskInformation, ScheduledTaskInformationData];

ScheduledTaskInformation[obj_CloudObject] := iCloudScheduledTaskInformation[obj];

ScheduledTaskInformation::noprop = "`1` is not a property returned by ScheduledTaskInformation.";

ScheduledTaskInformation[obj_CloudObject, property_] :=
	iCloudScheduledTaskInformation[obj] /. {
		System`ScheduledTaskInformationData[assoc_] :>
			If[KeyExistsQ[assoc, property],
				assoc[property],
				(* else *)
				Message[ScheduledTaskInformation::noprop, property];
				$Failed
			],
		_ :> $Failed
	}

iCloudScheduledTaskInformation[obj_CloudObject] := Module[
	{raw},
	raw = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
		CloudSystem`Scheduling`Private`scheduledTaskInformationImpl[obj, "CloudObjects" -> True]
	];

	If[MatchQ[raw, $Failed | _CloudSystem`Scheduling`Private`scheduledTaskInformationImpl],
		Message[ScheduledTask::listing]; 
		$Failed,
		(* else *)
		formatRawTaskInfo[raw]
	]
];

formatRawTaskInfo[raw_List] := If[MatchQ[Lookup[raw, "Visible"], _], 
	System`ScheduledTaskInformationData[Association[
		"Name" -> Lookup[raw, "Name"],
		"UUID" -> Lookup[raw, "TaskId"],
		"Expression" -> Lookup[raw, "Expression"],
		"LastRunDate" -> Lookup[raw, "LastTimestamp"],
		"NextRunDate" -> Lookup[raw, "NextTimestamp"],
		"StartDate" -> Lookup[raw, "StartTimestamp"],
		"EndDate" -> Lookup[raw, "EndTimestamp"],
		"CronSchedule" -> Lookup[raw, "CronSchedule"],
		"RepeatInterval" -> Lookup[raw, "RepeatInterval"],
		"RepeatCount" -> Lookup[raw, "RepeatCount"],
		"Paused" -> Lookup[raw, "Paused"],
		"Completed" -> Lookup[raw, "Completed"],
		"Status" -> Lookup[raw, "Status"],
		TimeZone -> Lookup[raw, TimeZone],
		NotificationOptions -> Lookup[raw, NotificationOptions],
		(*"MessageNotification" -> Lookup[raw, "MessageNotification"],*)
		"Active" -> Lookup[raw, "Active"],
		AutoRemove -> Lookup[raw, AutoRemove]
	]],
	(* else *)
	Null
]


CloudObject /: ScheduledTaskActiveQ[co_CloudObject, o:OptionsPattern[]] /; documentGeneratorQ[co] := Module[
	{i = System`DocumentGeneratorInformation[co, "Task"]},
	If[MatchQ[i, _ScheduledTaskInformationData],
		i[[1, "Active"]] && Not[i[[1, "Paused"]]],
		(* else *)
		i
	] 
]

CloudObject /: ScheduledTaskActiveQ[task_CloudObject] := With[
	{i = iCloudScheduledTaskInformation[task]},
	If[MatchQ[i, _ScheduledTaskInformationData],
		i[[1, "Active"]] && Not[i[[1, "Paused"]]],
		(* else *)
		i
	] 
]

(* Not presently documented *)
CloudObject /: NextScheduledTaskTime[co_CloudObject, o:OptionsPattern[]] /; documentGeneratorQ[co] := Module[
	{res = Catch[iCloudNextDocumentGeneratorTime[co, o], $tag]},
	res
]

CloudObject /: NextScheduledTaskTime[task_CloudObject] := With[
	{res = Catch[iCloudNextScheduledTaskTime[task], $tag]},
	res
]

iCloudNextScheduledTaskTime[co:CloudObject[uri_String, ___]] := Module[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`Private`scheduledJob],
		Message[ScheduledTask::nonext, co];
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
	]
]

iCloudNextScheduledTaskTime[st_, OptionsPattern[]] := (Message[ScheduledTask::nonext, st]; $Failed)

GetNameFromURI[uri_String] := With[{split=StringSplit[uri,"/"]},
	If[Length[split]<2,Message[ScheduledTask::nouri,uri];Throw[$Failed,$tag],Last[split]]
]

GetUUID[obj_CloudObject] := Module[{res},If[MatchQ[res=getCloudAndUUID[obj],{_,id_String}],Last[res],Throw[$Failed,$tag]]]
GetUUID[obj_String] := GetUUID[CloudObject[obj]]
GetUUID[___] := Throw[$Failed,$tag]


Protect[ScheduledTask, CloudObject, ScheduledTasks, EvaluateScheduledTask, ScheduledTaskInformation, ScheduledTaskInformationData];

$Flag = False;

End[]

EndPackage[]
