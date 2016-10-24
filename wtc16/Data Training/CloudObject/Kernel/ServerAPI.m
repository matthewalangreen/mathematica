(* Mathematica package *)
BeginPackage["CloudObject`"]

General::maxviewers = "Maximum number of `1` viewer seats exceeded.";
General::userunknown = "User `1` unknown to the Wolfram Cloud.";
General::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again.";
General::notperm = "Unable to perform the requested operation. Permission denied.";
General::unavailable = "The cloud service is not available. Please try again shortly.";
General::notparam = "Invalid parameters were specified.";
General::notmethod = "The specified method is not allowed.";
General::rejreq = "The specified request was rejected by the server.";(*rate limit exceeded, etc*)
General::srverr = "An unknown server error occurred.";
General::cloudnf = "No CloudObject found at the given address"; (* we would like an argument: "No CloudObject at `1`."; but it requires some refactoring *)
General::cloudprecondition = "A precondition check in the cloud service failed.";
General::cloudunknown = "An unknown error occurred.";

Begin["`Private`"]

handleErrorDetails["max-viewers", extra_, head_] := Message[head::maxviewers, extra];
handleErrorDetails["unknown-user", extra_, head_] := Message[head::userunknown, extra];
handleErrorDetails[code_, extra_, head_] := Message[head::notparam];

(*TODO: handle various error codes*)
checkError[response_, msghd_Symbol:CloudObject] :=
    If[response === {$Failed, $Failed},
        True,
        With[{res = MatchQ[response, HTTPError[_Integer, ___]]},
            If[res,
                Switch[response,
                    HTTPError[400 | 412, {__Rule}, ___],
                        (* When the server returns a 400 or 412, it sometimes returns JSON data in the response body,
                         giving more details about the error. *)
                        handleErrorDetails["errorCode" /. response[[2]], "extraData" /. response[[2]], msghd],
                    HTTPError[400, ___], Message[msghd::notparam],
                    HTTPError[401, ___], Message[msghd::notauth],(*might need a different message here*)
                    HTTPError[403, ___], Message[msghd::notperm],
                    HTTPError[404, ___], Message[msghd::cloudnf], (* TODO need a CloudObject to pass here *)
                    HTTPError[405, ___], Message[msghd::notmethod],
                    HTTPError[412, ___], Message[msghd::cloudprecondition],
                    HTTPError[429, ___], Message[msghd::rejreq],
                    HTTPError[500, ___], Message[msghd::srverr],
                    HTTPError[503, ___], Message[msghd::unavailable],
                    _, Message[msghd::cloudunknown]
                ]
            ];
            res
        ]
    ]

fetchURL[url_, elements_, options___] := If[TrueQ[$CloudConnected], authenticatedURLFetch, URLFetch][url, elements, options]

(* The asynchronous version simply ignores the requested elements. It does not return anything, it just sets off the request. *)
fetchURLAsync[url_, elements_, options___] := (
    If[TrueQ[$CloudConnected], authenticatedURLFetchAsynchronous, URLFetchAsynchronous][url, Null, options];
    {200, {}, {}}
)

contentDisplay[list:{_Integer...}] := FromCharacterCode[list]
contentDisplay[value_String] := value
contentDisplay[expr_] := ToString[expr, InputForm]

preprocessContent[content_List] := FromCharacterCode[content, "UTF-8"]
preprocessContent[content_] := content

preprocessErrorContent[content_, "application/json"] :=
    Module[{data},
        data = ImportString[preprocessContent[content], "JSON"];
        log["Error content: `1`", data, DebugLevel->2];
        data
    ]
preprocessErrorContent[content_, type_] := preprocessContent[content]

callServer[url_, mimetype_: "text/plain", httpVerb_: "GET", body_: {}, async_ : False] := Module[{response,status, headers, content, callFunction, finalURL, contentType},
   log["Calling remote server: `1` `2` with MIME type `3`", httpVerb, url, mimetype];
   log["Decoded URL: `1`", URLDecode[url], DebugLevel->2];
   log["Request content: `1`", contentDisplay[body], DebugLevel->2];
   (* Otherwise, check for actual authentication. *)
   If[Not[TrueQ[authenticatedQ[]]],
       With[{res=CloudConnect[]}, (*TODO: what to do for stand-alone kernel? *)
           If[UnsameQ[res, $WolframID], Message[CloudObject::notauth]; Return[HTTPError[401]]]
       ]
   ];
   finalURL = url;
   callFunction = If[async, fetchURLAsync, fetchURL];
   response = callFunction[finalURL, {"StatusCode", "Headers", "ContentData"},
       "Method"->httpVerb, "Headers"->{"Content-Type"->mimetype}, "BodyData"->body,
       "VerifyPeer"->False
   ];
   If[MatchQ[response,{_,_,_}], {status, headers, content} = response, Return[HTTPError[404]]];
   log["Response status: `1`", status];
   If[headers =!= {},
       log["Response headers: `1`", headers, DebugLevel->2];
   ];
   log["Response content: `1`", contentDisplay[content], DebugLevel->2];
   contentType = "Content-Type" /. (Rule @@@ headers);
   If[status =!= 200,
       content = preprocessErrorContent[content, contentType];
       Return[HTTPError[status, content, contentType]]
   ];
   {contentType, content}
]

getUUID[cloud_, path_] := Module[{pathString, uuid},
    pathString = JoinURL[path];
    uuid = responseToString @ execute[cloud, "GET", {"files"}, Parameters -> {"path" -> pathString}];
    log["UUID for path `1`: `2`", pathString, uuid];
    If[uuid === "", None, uuid]
]

getCloud[uri_] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        cloud
    ]

getCloudAndUUID[obj : CloudObject[uri_, ___]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[uuid === None,
            uuid = getUUID[cloud, {user, path}],
        (* uuid set, check for path inside it (file inside an unnamed directory) *)
            If[extraPath =!= {},
                uuid = getUUID[cloud, {uuid, extraPath}]
            ]
        ];
        {cloud, uuid}
    ]

getCloudAndUUIDOrPath[CloudObject[uri_, ___]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[extraPath === {},
            {cloud, uuid, If[path === None, None, Join[{user}, path]]},
        (* else: *)
            If[uuid === None,
            (* this will not actually happen, because extraPath is only set when uuid is set *)
                {cloud, None, Join[{user}, path, extraPath]},
            (* else *)
                {cloud, None, Join[{uuid}, extraPath]}
            ]
        ]
    ]

getCloudAndPathList[obj_CloudObject] :=
    Module[{cloud, uuid, path},
        {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
        {cloud, If[path === None, {uuid}, path]}
    ]

Options[execute] = {Parameters -> {}, Body -> {}, Type -> "text/plain", UseUUID -> True, Asynchronous -> False};

(* perform the execute locally, we are already in the cloud *)
Options[executeInCloud] = Options[execute];
executeInCloud[cloud_String, method_String, path_List : {}, OptionsPattern[]] :=
	Module[{parameters, mimetype = OptionValue[Type], body = OptionValue[Body],
		bodyString},

		parameters = OptionValue[Parameters];

		log["Calling server `1` `2` with MIME type `3`, parameters `4`", method,
			JoinURL[path], mimetype, ToString[parameters, InputForm],
			DebugLevel -> 2];
		If[body =!= {},
			log["Content: `1`", body, DebugLevel->2];
		];
		(* string is more efficient than a list of bytes in the Java server *)
		bodyString = If[ListQ[body], FromCharacterCode[body], body];

        $lastExecuteResult = CloudSystem`Private`writeCallPacketService[
            CloudSystem`CloudObject`DoCloudOperation[method, path, parameters,
                mimetype, bodyString
            ]
        ];
        log["Call packet result: `1`", ToString[$lastExecuteResult, InputForm], DebugLevel -> 2];

        $lastExecuteResult /. {
			{type_String, resultFile_String} :>
				{type, BinaryReadList[resultFile]},
			finalResult : {_String, _List} :> finalResult,
			err:HTTPError[status_Integer?Positive, content_, type_] :>
                HTTPError[status, preprocessErrorContent[content, type], type],
            err:HTTPError[_Integer?Positive] :> err,
			_ :> HTTPError[500]
		}
	]

(* make an HTTP request to perform the execute *)
Options[executeRemotely] = Options[execute];
executeRemotely[cloud_String, method_String, path_List : {}, OptionsPattern[]] := Module[{url},
    url = JoinURL[{cloud, path}] <> JoinURLSearch[OptionValue[Parameters]];
    callServer[url, OptionValue[Type], method, OptionValue[Body], OptionValue[Asynchronous]]
]

execute[cloud_String, method_String, path_List : {}, opts:OptionsPattern[]] :=
	If[TrueQ[System`$CloudEvaluation],
		executeInCloud[cloud, method, path, opts]
		,
		executeRemotely[cloud, method, path, opts]
	]

execute[obj_CloudObject, method : _String | Automatic : "GET", api_String : "files", subpath_List : {}, options : OptionsPattern[]] :=
    Module[{cloud, uuid, path, methodToUse},
        If[OptionValue[UseUUID] === True,
	        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
	        If[!StringQ[uuid], Message[CloudObject::cloudnf, obj]; Return[{$Failed, $Failed}]];
	        log["Executing on UUID `1`", uuid];
	        execute[cloud, method, {api, uuid, subpath}, options],
	    (* else *)
            {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
            If[method === Automatic,
                If[uuid === None,
                    methodToUse = "POST",
                    methodToUse = "PUT"
                ]
            ];
            If[uuid === None,
	            execute[cloud, methodToUse, {api, subpath}, Parameters -> Join[OptionValue[Parameters], {"path" -> JoinURL[path]}], options],
	            execute[cloud, methodToUse, {api, uuid, subpath}, options]
            ]
        ]
    ]

responseToString[{type_, content_List}, head_] := FromCharacterCode[content, "UTF-8"]
responseToString[{type_, content_String}, head_] := content
responseToString[response_, head_] := $Failed /; checkError[response, head]
responseToString[response_] := responseToString[response, CloudObject]

responseToExpr[response_] := responseToString[response] /. r_String :> ToExpression[r]

responseToStringList[response_, head_] := StringSplit[responseToString[response, head], "\n"]
responseToStringList[response_, head_] := $Failed /; checkError[response, head]

dumpBinary[filename_, contents_] := Module[{file},
    file = OpenWrite[filename, BinaryFormat -> True];
    BinaryWrite[file, contents];
    Close[file];
]

responseToFile[{type_, content_List}, head_:CloudObject] := Module[{tempfilename},
	tempfilename = CreateTemporary[];
	dumpBinary[tempfilename, content];
	{tempfilename, type}
]
responseToFile[response_, head_:CloudObject] := {$Failed, $Failed} /; checkError[response, head]

responseCheck[response_, head_, result_] :=
    If[checkError[response, head],
        $Failed,
        result
    ]
responseCheck[response_, head_ : CloudObject] := responseCheck[response, head, Null]

End[]

EndPackage[]
