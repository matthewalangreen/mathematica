BeginPackage["CloudObject`"]

System`CloudLoggingData;

Begin["`Private`"]

(* CloudLoggingData *)

CloudLoggingData[obj_CloudObject] :=
    Module[{data},
        data = getLoggingData[obj] /. {$Failed :> Return[$Failed]};
        createLoggingData[data]
    ]

CloudLoggingData[head_Symbol, more___] := 
	CloudLoggingData[deploymentId[head], more]

CloudLoggingData[
	deployment:("API" | "Form" | "Computation" | "Task" | "CloudCDF"), 
	more___] := 
	deploymentLoggingData[deployment, more]

deploymentId[head_] := 
	head /. {
		APIFunction -> "API",
		FormFunction -> "Form",
		Delayed -> "Computation",
		ScheduledTask -> "Task",
		Notebook -> "CloudCDF"
	}

(*****************************************************************************)

createLoggingData[data_List] := 
	Module[{logs, rawdata, callDetailRawData, creditsBilledRawData},
        logs = $lastLoggingDataLogsData = Lookup[data, "logs"];
        rawdata = $lastLoggingDataRawData =
            Cases[Map[parseLogEntry, logs], _Association];

        callDetailRawData = Select[rawdata, isBillingEvent];
        creditsBilledRawData = Select[rawdata, #["channel"]==="cloud:resources:user:credits"&];
        createLoggingDataFromRawData[callDetailRawData, creditsBilledRawData]
	]

isCallEvent[event_Association] := isCallEvent[event["channel"]]

isCallEvent[channel_String] := StringMatchQ[channel, "cloud:event:" ~~ ___]

isBillingEvent[event_Association] := isBillingEvent[event["channel"]]

isBillingEvent[channel_String] := 
	StringMatchQ[channel, ___ ~~ "billing:usage:cloud:credits" ~~ ___]

(* Create LoggingData Association from two lists of parsed events, 
	callDetailRawData and creditsBilledRawData.
*)
createLoggingDataFromRawData[callDetailRawData_, creditsBilledRawData_] := 
	Module[{callDetails, creditsBilledDetails},
        callDetails = Cases[Map[createCallDetail, callDetailRawData], _Association];
        creditsBilledDetails = Cases[Map[createCallDetail, creditsBilledRawData], _Association];
        createLoggingDataFromDetails[callDetails, creditsBilledDetails]
	]

(* Create LoggingData Association from lists in the format of CallDetails and 
	CreditUsage.
*)
createLoggingDataFromDetails[callDetails_, creditsBilledDetails_] := 
	Module[{callCountTimeSeries, callHistory, totalCalls, 
		creditsBilledTimeSeries, creditUsageHistory, totalCreditsUsed},

        callCountTimeSeries = quietTimeSeries@createLoggingDataTimeSeries[callDetails, 1&];
        callHistory = quietTimeSeries@createTimeSeriesHistory[callCountTimeSeries];
        totalCalls = quietTimeSeries@createTotalsSummary[callHistory];

        creditsBilledTimeSeries = quietTimeSeries@createLoggingDataTimeSeries[creditsBilledDetails, #["Credits"]&];
        creditUsageHistory = createTimeSeriesHistory[creditsBilledTimeSeries];
        totalCreditsUsed = createTotalsSummary[creditUsageHistory];

        <|
            "TotalCalls" -> totalCalls,
            "CallHistory" -> callHistory,
            "CallDetails" -> callDetails,
            "CreditUsageHistory" -> creditUsageHistory,
            "TotalCreditsUsed" -> totalCreditsUsed,
            "CreditUsageDetails" -> creditsBilledDetails
        |>
	]

deploymentLoggingData[deployment_] := 
	deploymentLoggingData[deployment, {{2014, 3, 22, 0, 0, 0.`}, DateList[]}]

deploymentLoggingData[deployment_, {startDate_, endDate_}] := 
	Module[{channels, data},
		channels = Join[callEventsForDeployment[deployment], 
			$creditsBilledChannels];
        data = getLoggingDataForChannelsAndDateRange[deployment, channels,
        	startDate, endDate] /. {$Failed :> Return[$Failed]};
        createLoggingData[data]
	]

callEventsForDeployment[type_] := callEventsForDeployment[type] = 
	With[{events = Map[#<>":billing:usage:cloud:credits:"<>
		billingEventID[type]<>":time"&, productPrefixes]
		(* Map[#<>":cloud:event:"<>callEventID[type]&, productPrefixes] *)},
		If[type === "Computation" || type === "Form",
			Join[events, callEventsForDeployment["API"]],
			events
		]
	]

makeProductPrefixes[channel_] := 
	Map[#<>":"<>channel&, productPrefixes]

productPrefixes = {"www", "wpc", "programmingcloud", "unknown"}

$creditsBilledChannels = makeProductPrefixes["cloud:resources:user:credits"]

callEventID[deployment:("API" | "Form" | "Computation" | "Task")] := 
	callEventID[deployment] = ToLowerCase[deployment]

callEventID[deployment:"PublishedPage"] := callEventID[deployment] = "cloudcdf"

billingEventID[deployment_] := billingEventID[deployment] = 
	deployment /. {
		("API" | "Form" | "Computation") -> "instantapi",
		"Task" -> "scheduledtask",
		"PublishedPage" | "CloudCDF" -> "notebook",
		other_ :> other
	}

getLoggingData[obj_CloudObject] := 
	Module[{cloud, uuid},
		{cloud, uuid} = getCloudAndUUID[obj];
		getLoggingData[cloud, uuid]
	]

getLoggingData[cloud_String, uuid_String] := 
	execute[cloud, "GET", {"files", uuid, "logs"}] /. {
		err_HTTPError :> (
			checkError[$lastLoggingDataError = err, CloudLoggingData];
			$Failed),
		{_, bytes_List} :> ImportString[FromCharacterCode[bytes], "JSON"]
	}

getLoggingDataForChannelsAndDateRange[deployment_, channels_List, start_List, 
	end_List] := 
	execute[$CloudBase, "GET", {"logs"}, 
		Parameters -> Join[Map["channel" -> # &, channels], {
			"deploymentType" -> toServerDeploymentType[deployment], 
			"start" -> ToString@toInterconnectTime[start], 
			"end" -> ToString@toInterconnectTime[end]}]] /. {
		err_HTTPError :> (
			checkError[$lastLoggingDataError = err, CloudLoggingData];
			$Failed),
		{_, bytes_List} :> ImportString[FromCharacterCode[bytes], "JSON"]
	}

toServerDeploymentType[deployment_] := ToUpperCase[deployment]

SetAttributes[quietTimeSeries, HoldAll]

quietTimeSeries[expr_] := Quiet[expr, Interpolation::inhr]

createCallDetail[entry_] := createCallDetail[entry["channel"], entry]

createCallDetail[channel_String?isBillingEvent, entry_] :=
	With[{data = entry["data"]},
		<|
		"Time" -> DateObject[entry["time"]],
		"UUID" -> Lookup[data, "fileId"]
		(* user metadata *)
		|>
	]

createCallDetail["cloud:event:api", entry_] :=
<|
"Time" -> DateObject[entry["time"]],
"UUID" -> entry["fileId"]
(* user metadata *)
    |>

createCallDetail["cloud:resources:user:credits", entry_] := 
	With[{data = entry["data"]},
		<|
			"Time" -> DateObject[entry["time"]],
			"UUID" -> Lookup[data, "fileId"],
			"Credits" -> Abs[data["credits"] /. {
				numtext_String :> parseNumber[numtext]
			}]
		|>
	]

parseNumber[numtext_] := 
	FromDigits@StringReplace[numtext,	StartOfString ~~ "-" -> ""]
	(* was using Interpreter["Number"], but it's not stable right now *)

(* discard unhandled event types *)
createCallDetail[_, _] := None

createLoggingDataTimeSeries[{}, _] := TimeSeries[{{Now, 0}}]

createLoggingDataTimeSeries[callDetails_, valueOf_] :=
    With[{details = Map[{#["Time"], valueOf[#]}&, callDetails]},
		TimeSeries[extendTimeSeriesToNow[details], 
			"DuplicateProcessingFunction" -> Total]
       ]

extendTimeSeriesToNow[{}] := {}

extendTimeSeriesToNow[ts_List] :=
    With[{now = Now},
        If[ts[[-1, 1]] === now, (* it's already up-to-date, leave it alone *)
            ts,
        (* last data point is older than now, add a point for now *)
            Append[ts, {now, 0}]
        ]
    ]

createTimeSeriesHistory[timeSeries_] :=
(* build up the call history by aggregating in increasing granularity,
		using the smaller time series to aggregate the next largest *)
    Fold[
        Function[{acc, elt},
            With[{history = First[acc], key = First[elt], unit = Last[elt]},
                With[{ts2 = TimeSeriesAggregate[Last[acc], {1, unit}, Total]},
                    {Append[history, key -> ts2], ts2}
                ]
            ]
        ],
        {<| |>, timeSeries},
        {{"Hourly", "Hour"}, {"Daily", "Day"}, {"Weekly", "Week"},
            {"Monthly", "Month"}, {"Yearly", "Year"}}
    ] // First (* return the final association *)

createTotalsSummary[<| |>] :=
Append[
    Association[Map[# -> 0&, $loggingDataTotalCallsLabels]],
    "All" -> 0
]

createTotalsSummary[callHistory_Association] :=
    Append[
        Association@Map[
            #[[1]] -> Last[callHistory[#[[2]]]["Values"]]&,
            $loggingDataTotalCallsKeys
        ],
        "All" -> Total[callHistory["Yearly"]["Values"]]
    ]

$loggingDataTotalCallsKeys = {
    {"LastHour", "Hourly"}, {"LastDay", "Daily"}, {"LastWeek", "Weekly"},
    {"LastMonth", "Monthly"}, {"LastYear", "Yearly"}
}

$loggingDataTotalCallsLabels = Map[First, $loggingDataTotalCallsKeys]

loggingDataEventQ[channel_] :=
	isBillingEvent[channel] || isCallEvent[channel] || 
	channel === "cloud:resources:user:credits"

parseLogEntry[entry_] :=
    With[{data = {"timestamp", "channel", "message"} /. entry},
        With[{channel = removeProductFromChannel[data[[2]]]},
            If[loggingDataEventQ[channel],
                Association[
                    "time" -> fromInterconnectTime[data[[1]]],
                    "channel" -> channel,
                    "data" -> parseLogEntryMessage[data[[3]]]
                ],
                None
            ]
        ]
    ]

removeProductFromChannel[channel_] :=
    StringReplace[channel, {StartOfString ~~ (Except[":"] ..) ~~ ":" -> ""}]

parseLogEntryMessage[msg_] :=
    ImportString[msg, "JSON"] /. { (* TODO convert message on the server side *)
    (* only the data field of the message object is needed *)
        data_List :> Association[Evaluate["data" /. data]]
    }

fromInterconnectTime[interconnectTimestamp_] := 
	DateObject[unixTimeMillisToAbsoluteTime[interconnectTimestamp], 
		TimeZone -> 0]

unixTimeMillisToAbsoluteTime[unixTimeMillis_] := 
	unixTimeToAbsoluteTime[unixTimeMillis / 1000.]

unixTimeToAbsoluteTime[unixTime_] := unixTime + absoluteTimeEpochOffset

absoluteTimeToInterconnectTime[abstime_] := 
    Round[1000*(abstime - absoluteTimeEpochOffset)]

toInterconnectTime[abstime_?NumberQ] := 
	(* TODO this will round trip from abstime to DateObject just to do a timezone
		conversion, we should really just do the TZ conversion here *)
	toInterconnectTime[DateObject[abstime]]  

toInterconnectTime[dl_List] := 
	toInterconnectTime[DateObject[dl]]

toInterconnectTime[date_DateObject] := 
	absoluteTimeToInterconnectTime[AbsoluteTime[date, TimeZone -> 0]]

dateListToInterconnectTime[dl_List] :=
	absoluteTimeToInterconnectTime[AbsoluteTime[dl]]

absoluteTimeEpochOffset = 25567*86400 (* #seconds from 1/1/1900 to 1/1/1970 *)

totalDeployments[] :=
    Module[{deployed = deployedObjects[]},
        <|
            "API" -> selectDeployed["application/vnd.wolfram.expression.api", deployed],
            "Forms" -> selectDeployed["application/vnd.wolfram.expression.form", deployed]
        |>
    ]

(* TODO this is a workaround until the server side can fill in the correct mime type in the GET /objects/deployments  *)
selectDeployed[mimetype_, deployed_] :=
    Select[CloudObject`Private`CloudObjectsByType[mimetype],
        KeyExistsQ[deployed, #]&]

deployedObjects[] :=
    Module[{filesdata},
        filesdata = CloudObject`Private`execute[$CloudBase, "GET",
            {"objects", "deployments"}] /. {
            {_, bytes_List} :> ImportString[FromCharacterCode[bytes], "JSON"]
        };
        Association@
            Map[With[{uuid = "uuid" /. #}, cloudObjectFromUUID[uuid] -> uuid]&,
      			Lookup[filesdata, 
					If[KeyExistsQ[filesdata, "directoryListing"], 
						"directoryListing", 
						"files"]]
			]
    ]

aggregateObjects[rules_] :=
    With[{baseData = Map[With[{data = Last[#]},
        KeyTake[data, {"TotalCalls", "CallHistory"}]] &, rules]},
        <|
            "TotalCalls" -> Fold[accumulate, <||>, Map[#["TotalCalls"] &, baseData]]
            (* TODO CallHistory *)
        |>
    ]

(* given a list of Associations and an initial accumulator Association (which
	could be empty), return an Association where each key has had combine
	applied to the corresponding key from each object in the list.
*)
accumulate[accumulator_Association, item_Association, combine_: Plus,
    default_: 0] :=
    Fold[With[{acc = #, key = #2[[1]], value = #2[[2]]},
        Append[acc, key -> combine[Lookup[acc, key, default], value]]
    ]&,
        accumulator,
        assocToRuleList[item]
    ]

assocToRuleList[assoc_Association] := Map[# -> assoc[#] &, Keys[assoc]] (* work around a bug in Normal[_Association] *)

End[]

EndPackage[]
