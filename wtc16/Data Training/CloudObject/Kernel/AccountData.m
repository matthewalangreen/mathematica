BeginPackage["CloudObject`"]

System`CloudAccountData
System`$CloudCreditsAvailable

Begin["`Private`"]

CloudAccountData[property_] := 
	CloudAccountData[] /. {
		data_Association :> data[property],
		_ :> $Failed
	}

CloudAccountData[] := 
	Module[{json, data, basic, subscriptions, storageLimit, storageUsedBytes, 
		storageUsedMB},
		json = $lastCloudAccountDataJSON = 
			execute[$CloudBase, "GET", {"REST", "user", "subscriptions"}] /. {
				{_, bytes_List} :> FromCharacterCode[bytes],
				other_ :> (
					checkError[other, CloudAccountData];
					Return[$Failed]
				)
		};

		data = $lastCloudAccountData = ImportString[json, "JSON"];
		If[!validAccountDataQ[data],
			Message[CloudAccountData::srverr];
			Return[$Failed]
		];

		basic = Lookup[data, "sharedObject"];
		subscriptions = Lookup[data, "subscriptions"];

		storageLimit = Quantity[
			interpretLimit["Number", Lookup[basic, "maxStorage"]], "Megabytes"];

		storageUsedBytes = Quantity[
			interpretLimit["Number", Lookup[basic, "usedStorage"]] /. {
				Except[_Integer] :> 0
			},
			"Bytes"];
		storageUsedMB = Round@UnitConvert[storageUsedBytes, "Megabytes"];

		<|
			"Products" -> Map[createProduct, subscriptions],
			"CloudStorage" -> storageLimit,
			"CloudStorageUsed" -> storageUsedMB,
			"CloudStorageAvailable" -> 
				Max[Quantity[0, "Megabytes"], storageLimit - storageUsedMB],
			"CloudCreditsAvailable" -> Lookup[basic, "credits", Indeterminate],
			"WolframAlphaCallsAvailable" -> 
				Lookup[basic, "wolframAlphaCalls", Indeterminate]
		|>
	]

validAccountDataQ[data_] := 
	MatchQ[data, _Association | _List] &&
	KeyExistsQ[data, "sharedObject"] && KeyExistsQ[data, "subscriptions"]

If[!MemberQ[Attributes@$CloudCreditsAvailable,Locked],
	Unprotect[$CloudCreditsAvailable];
	$CloudCreditsAvailable := 
		With[{credits = CloudAccountData["CloudCreditsAvailable"]},
			If[TrueQ[$CloudConnected],
				credits,
				Indeterminate
			]
		];
	Protect[$CloudCreditsAvailable];
]
(*
	If[TrueQ[$CloudConnected],
		CloudAccountData["CloudCreditsAvailable"],
		Indeterminate
	]
*)

createProduct[subscription_] := 
	Module[{info = Lookup[subscription, "planInfo"], product, 
		featureMap = createFeatureMap[Lookup[subscription, "planInfo"]],
		limitQuantity},

		product = Lookup[info, "productInfo"];
		
		limitQuantity = Quantity[featureMap[#, "limitValue"], 
			normalizeUnits[StringTrim[featureMap[#, "unit"]]]]&;

		<|
			"Product" -> Lookup[product, "name"],
			"Plan" -> Lookup[info, "name"],
			"StartDate" -> formatAccountDate[Lookup[subscription, "startDate"]],
			"EndDate" -> 
				formatAccountDate[Lookup[subscription, "finalAccessDate"]],
			"NextBillingDate" -> 
				formatAccountDate[Lookup[subscription, "nextBillingDate"]],
			"CloudStoragePoolable" -> limitQuantity["storage"],
			"CloudCreditsRecurring" -> limitQuantity["cloudcredits"],
			"CloudCreditsPurchasingAllowed" ->
				TrueQ[featureMap["addcloudcredits", "limitValue"]],
			"DeveloperSeats" -> featureMap["devseats", "limitValue"],
			"TechnicalSupportType" -> featureMap["techsupport", "limitValue"],
			"DesktopAccessAllowed" -> 
				TrueQ[featureMap["desktop", "limitValue"]],
			"LocalFileAccessAllowed" -> 
				TrueQ[featureMap["localfileaccess", "limitValue"]],
			"FileSizeLimit" -> limitQuantity["filesize"],
			"SessionEvaluationTimeLimit" -> limitQuantity["complength"],
			"SessionMemoryLimit" -> limitQuantity["sessionmem"],
			"DeployedEvaluationTimeLimit" -> limitQuantity["publiccomplength"],
			"DeployedMemoryLimit" -> limitQuantity["publicsessionmem"],
			"ScheduledTaskEvaluationTimeLimit" -> 
				limitQuantity["servicecomplength"],
			"ScheduledTaskMemoryLimit" -> limitQuantity["servicesessionmem"]
		|>
	]

formatAccountDate[date_Integer] := fromAccountTime[date]
formatAccountDate[_] := None

createFeatureMap[planInfo_] := 
	Association@Map[
  		With[{info = Association[Lookup[#, "cloudPlanFeatureInfo"]]}, 
    		Lookup[info, "shortName"] -> 
			Association@Append[info, 
				"limitValue" -> 
					interpretLimit[info["unitType"], Lookup[#, "limitValue"]]]
		]&,
		Lookup[planInfo, "planFeatureLimitsInfo"]
	]

fromAccountTime[accountTimestamp_] := 
	DateObject[unixTimeMillisToAbsoluteTime[accountTimestamp], TimeZone -> -6]

interpretLimit[unitType_, value_] := 
	Interpreter[unitType /. {"Integer" -> "Number"}][value]

(* Make unit specifications canonical so Quantity doesn't need a network call *)
normalizeUnits[units_] := 
	StringTrim[units] /. {
		"/month" -> 1/"Months",
		"seconds" -> "Seconds",
		"MB" -> "Megabytes",
		"GB" -> "Gigabytes"
	}

End[]
EndPackage[]
