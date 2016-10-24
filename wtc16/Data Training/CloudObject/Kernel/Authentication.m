(* Mathematica package *)
BeginPackage["CloudObject`"]

System`CloudConnect;
System`CloudDisconnect;
System`$CloudConnected;
System`$WolframID;
System`$WolframUUID;
System`$RegisteredUserName;
$CloudDebug;
$CloudDebugLevel;

Begin["`Private`"]

Unprotect[CloudConnect];
Unprotect[CloudDisconnect];
Unprotect[$CloudConnected];

CloudConnect::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again."
CloudConnect::oauth = "Unrecognized authentication information. Please contact technical support for assistance.";
CloudConnect::creds = "Incorrect username or password.";
CloudConnect::nocrd = "No username or password sent.";
CloudConnect::apkey = "Unrecognized authentication keys. Please contact technical support for assistance.";
CloudConnect::badts = "Invalid Timestamp. Please ensure your system clock is set to the correct time.";

$AuthenticationMethod = "XAuth";
$tag = "CloudObjectCatchThrowTag";
$Flag = True;
If[Not[ValueQ[$CloudConnected]],Set[$CloudConnected,TrueQ[$CloudEvaluation]]];

RememberMeValueFromDialogCheckbox[]:= If[SameQ[Head[$FrontEnd],FrontEndObject],
	TrueQ[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "DialogSettings", "Login", "RememberMe"}]],
	True
]

Options[CloudConnect] = {"RememberMe"->Automatic, "Prompt"->True};

CloudConnect[args___,OptionsPattern[]]:=Block[{$hasFailed=False},With[{res=Catch[
	With[{ov=Check[OptionValue["RememberMe"],Throw[$Failed,$tag],{OptionValue::nodef,OptionValue::optnf}],
		prompt=OptionValue["Prompt"]},
	If[
		Not[MatchQ[ov,Automatic|True|False]],
		Message[CloudConnect::opttf,"RememberMe",ov];Throw[$Failed,$tag]
	];
	If[$Notebooks==True,(*if FrontEnd is present*)
	MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionInitiated[]];
	Block[{$CacheLogin = TrueQ[OptionValue["RememberMe"]/.Automatic->RememberMeValueFromDialogCheckbox[]]},
	iConnectAndVerify[{args},prompt]]],
	$tag]},
	(
	If[$Notebooks==True,(*if FrontEnd is present*)
		If[SameQ[$WolframID, None], (*if user didn't log in*)
			FEConnectFail[1700],
			With[{cloudbase = $CloudBase, username = $WolframID, displayname = $RegisteredUserName, uuid = $WolframUUID},
				MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionEstablished[cloudbase, username, displayname, uuid]]
		]];
	res)/;MatchQ[res,$WolframID|$Failed|$Canceled]]]/;Not[TrueQ[$CloudEvaluation]]

pingCloudServer[]:=With[{r=authenticatedURLFetch[
		If[SameQ[StringTake[$CloudBase,-1],"/"],
			StringJoin[$CloudBase,"files/auth"],
			StringJoin[$CloudBase,"/files/auth"]
		],"StatusCode","VerifyPeer"->False]},(*ping server to verify credentials are still valid*)
		If[
			UnsameQ[r,200],(*TODO: needs more error handling*)
			Message[CloudConnect::notauth];CloudDisconnect[];Throw[$Failed,$tag],
			$WolframID
		]
	]

iConnectAndVerify[args___] := With[{res=iCloudConnect[args]},
	If[MatchQ[res,$WolframID],
		pingCloudServer[],
		res
	]
]

iCloudConnect[{},prompt:(True|False)] := With[{r=If[TrueQ[$CacheLogin],fetchCredentials[],$Failed]},
	If[FreeQ[r,$Failed],$WolframID,If[TrueQ[prompt],iCloudConnect[{""},True],$Failed]]]
iCloudConnect[{username_String},True] :=
	With[
		{r=If[TrueQ[$CacheLogin],fetchCredentials[],$Failed]},
		If[
			FreeQ[r,$Failed],
			If[SameQ[username,$WolframID],
				$WolframID,
				CloudDisconnect[];authenticate[username]
			],
			authenticate[username]
		]
	]

iCloudConnect[{username_String,password_String},_] := authenticate[username,password]
iCloudConnect[{args__},___] := (ArgumentCountQ[CloudConnect,Length[DeleteCases[{args},_Rule,Infinity]],0,2];Null/;False)
iCloudConnect[{___},False] := $Failed

flushCredentials[] := With[{username = $WolframID},
If[$Notebooks==True,(*if FrontEnd is available*)
		MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionTerminated[username]];
setCloudConnected[False];
setWolframID[None];
setWolframUUID[None];
setRegisteredUserName[""];
setAccessData["",""];
True
]

CloudDisconnect[] := Module[{},
	flushCredentials[];
	Quiet[DeleteFile[FileNameJoin[{$credsDir,$credsFile}]]];
]/;Not[TrueQ[$CloudEvaluation]]

CloudDisconnect[args__] := (ArgumentCountQ[CloudDisconnect,Length[{args}],0,0];Null/;False)

fetchCredentials[] := Catch[
	If[Not[FreeQ[getCredentials[],$Failed]],Throw[$Failed,$tag]];
	If[Not[StringQ[$CloudBase]],Throw[$Failed,$tag]]
	,$tag]


authenticate[__] /; SameQ[PacletManager`$AllowInternet,False] := (
FEConnectFail[1601];Message[CloudConnect::offline];$Failed)(*internet connectivity disabled*)
authenticate[username_String]:=With[{creds=loginDialog[username]},
    (*placeholder while we wait on real authentication*)
If[creds===$Canceled || Not[MatchQ[creds,{_String,_String}]],
    $Canceled,
    If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
    	If[TrueQ[$CacheLogin] || RememberMeValueFromDialogCheckbox[],storeCredentials[]];
    $WolframID,
    Message[CloudConnect::notauth];$Failed]]
]/;TrueQ[$Notebooks]

authenticate[username_String,password_String]:=With[{creds={username,password}},
    (*placeholder while we wait on real authentication*)
If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
		If[TrueQ[$CacheLogin](*TODO: re-check dialog status*),storeCredentials[]];
    $WolframID,
    Message[CloudConnect::notauth];$Failed]
]

authenticate[___] := $Failed

$randomCharacters = Join[
	CharacterRange["0", "9"],
	CharacterRange["A", "Z"],
	CharacterRange["a", "z"]
];


getServerFromCloudBase[base_String] := With[
	{server = StringCases[base, $UrlScheme<>"://www." ~~ ser___ ~~ "wolframcloud.com" ~~ ("/" | "") :> ser]},
  If[UnsameQ[server, {}], First[server], Throw[$Failed,$tag]]]
getServerFromCloudBase[___] := Throw[$Failed,$tag]

If[Not[ValueQ[$LocalhostAuthURL]], Set[$LocalhostAuthURL,"https://user.devel.wolfram.com/oauth/access_token"]];

getDomain[value_] :=
    StringReplace[value, ("http://" | "https://" | "") ~~ Shortest[domain_] ~~
        RegularExpression["(:[0-9]+)?(/.*)?"] :> domain]

makeAuthURL["localhost"] := $LocalhostAuthURL
makeAuthURL["localhost:8080/app"] := $LocalhostAuthURL
makeAuthURL["www.devel.wolframcloud.com"] := "https://user.devel.wolfram.com/oauth/access_token"
makeAuthURL["www.test.wolframcloud.com"] := "https://user.test.wolfram.com/oauth/access_token"
makeAuthURL["www.test2.wolframcloud.com"] := "https://user.test.wolfram.com/oauth/access_token"
makeAuthURL["datadrop.test.wolframcloud.com"] = "https://user.test.wolfram.com/oauth/access_token"
makeAuthURL["datadrop.wolframcloud.com"] = "https://user.wolfram.com/oauth/access_token"
makeAuthURL["www.wolframcloud.com"] := "https://user.wolfram.com/oauth/access_token"
makeAuthURL[url_]/;Not[TrueQ[$getDomainFlag]] := Block[{$getDomainFlag=True},makeAuthURL[getDomain[url]]]
makeAuthURL[___] := Throw[$Failed,$tag]

makeSignatureURL["localhost"] := "http://localhost"
makeSignatureURL["localhost:8080/app"] := "http://localhost:8080"
makeSignatureURL["www.devel.wolframcloud.com"] := "http://devel.wolframcloud.com"
makeSignatureURL["www.test.wolframcloud.com"] := "http://test.wolframcloud.com"
makeSignatureURL["www.test2.wolframcloud.com"] := "http://test2.wolframcloud.com"
makeSignatureURL["datadrop.test.wolframcloud.com"] = "http://datadrop.test.wolframcloud.com"
makeSignatureURL["datadrop.wolframcloud.com"] = "http://datadrop.wolframcloud.com"
makeSignatureURL["www.wolframcloud.com"] := "http://wolframcloud.com"
makeSignatureURL[url_]/;Not[TrueQ[$getDomainFlag]] := Block[{$getDomainFlag=True},makeSignatureURL[getDomain[url]]]
makeSignatureURL[___] := Throw[$Failed,$tag]

$authUrl := makeAuthURL[System`$CloudBase](*"https://user.test.wolfram.com/oauth/access_token"*)
$signatureURL := makeSignatureURL[System`$CloudBase](*"http://test.wolframcloud.com"*)
$oauthVersion = "1.0";
$unixtimebase = AbsoluteTime[{1970, 1, 1, 0, 0, 0}];
unixtime[] :=  Round[AbsoluteTime[TimeZone -> 0] - $unixtimebase];
nonce[] := StringJoin[RandomChoice[$randomCharacters, 20]]
$sigMethod = "HMAC-SHA1";

If[Not[ValueQ[$RegisteredUserName]],setRegisteredUserName[""]];

(*initial authentication*)
makeSubString[{username_String,password_String},{non_,time_}] := ExternalService`EncodeString[
	StringJoin["oauth_consumer_key=",getConsumerKey[],"&oauth_nonce=",non,"&oauth_signature_method=",$sigMethod,
		"&oauth_timestamp=",ToString[time],"&oauth_version=",$oauthVersion,
		"&x_auth_mode=client_auth&x_auth_password=",ExternalService`EncodeString[password],
		"&x_auth_username=",ExternalService`EncodeString[username]]]
(*subsequent requests*)
makeSubString[{non_,time_}] := ExternalService`EncodeString[
	StringJoin["oauth_consumer_key=",getConsumerKey[],"&oauth_nonce=",non,"&oauth_signature_method=",$sigMethod,
		"&oauth_timestamp=",ToString[time],"&oauth_token=",getAccessToken[],
		"&oauth_version=",$oauthVersion
	]]

(*initial authentication*)
makeSignatureBase[{username_String,password_String},{non_,time_}] := StringJoin[
	"POST&",ExternalService`EncodeString[$authUrl], "&",makeSubString[{username,password},{non,time}]]
(*subsequent requests*)
makeSignatureBase[{non_,time_},url_String,method_String] := StringJoin[
	method,"&",ExternalService`EncodeString[$signatureURL], "&",makeSubString[{non,time}]]

(*initial authentication*)
makeSignature[{username_String,password_String},{non_,time_}] := CloudHMAC[makeSignatureBase[{username,password},{non,time}],"Consumer"]
(*subsequent requests*)
makeSignature[{non_,time_},url_String,method_String] := CloudHMAC[makeSignatureBase[{non,time},url,method],"Access"]

(*initial authentication*)
makeOAuthHeader[{username_String,password_String}]:=With[{non=nonce[],time=unixtime[]},
StringJoin["OAuth realm=\"",$authUrl, "\", oauth_consumer_key=\"",getConsumerKey[],
	 "\", oauth_nonce=\"",non, "\", oauth_timestamp=\"",ToString[time],
	 "\", oauth_signature_method=\"",$sigMethod, "\", oauth_version=\"",$oauthVersion,
	 "\", oauth_signature=\"",makeSignature[{username,password},{non,time}],"\""]
]
(*subsequent requests*)
makeOAuthHeader[url_String,method_String]:=With[{non=nonce[],time=unixtime[]},
StringJoin["OAuth realm=\"",$signatureURL, "\", oauth_consumer_key=\"",getConsumerKey[],
	"\", oauth_token=\"",getAccessToken[],
	 "\", oauth_nonce=\"",non, "\", oauth_timestamp=\"",ToString[time],
	 "\", oauth_signature_method=\"",$sigMethod, "\", oauth_version=\"",$oauthVersion,
	 "\", oauth_signature=\"",ExternalService`EncodeString[makeSignature[{non,time},url,method]],"\""]
]

setWolframID[id:(_String|None)]:=(Unprotect[$WolframID];Set[$WolframID,id];Protect[$WolframID])
setWolframUUID[uuid:(_String|None)]:=(Unprotect[$WolframUUID];Set[$WolframUUID,uuid];Protect[$WolframUUID])
setCloudConnected[value:True|False] := (Unprotect[$CloudConnected];Set[$CloudConnected,value];Protect[$CloudConnected];value)
setRegisteredUserName[name:(_String)]:=(Unprotect[$RegisteredUserName];Set[$RegisteredUserName,name];Protect[$RegisteredUserName])

$CloudBase/:Set[$CloudBase,args__]/;And[Not[TrueQ[$CloudEvaluation]],Not[TrueQ[$Flag]]] := Block[{$Flag=True},
	flushCredentials[];Set[$CloudBase,args]
]


check401AndIssueMessage[content_] := Which[
	StringQ[content]&&StringMatchQ[content,"OAuth Verification Failed"~~__],
	Message[CloudConnect::oauth],

	StringQ[content]&&SameQ[content,"{\"error\":\"Incorrect username or password\"}"],
	Message[CloudConnect::creds],

	StringQ[content]&&StringMatchQ[content,"{\"error\":\"OAuth Verification (2) Failed: Timestamp is out of sequence."~~__],
	Message[CloudConnect::badts],

	True,(*TODO: have generic handler here*)
	Message[CloudConnect::creds]
]

FEConnectFail[status_Integer]/;UnsameQ[$hasFailed,True] := If[TrueQ[$Notebooks],
	Set[$hasFailed,True];
	MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionFailed[status]
]

fcc[arg_]:=FromCharacterCode[arg]

authenticateWithServer[{username_String, password_String},other_] := Catch[
 Module[{status, contentData, content},
 	status = Check[URLFetch[$authUrl, {"StatusCode", "ContentData"}, "Method" -> "POST",
 "Headers" -> {"Authorization" -> makeOAuthHeader[{username,password}]},
 "Parameters" -> {"x_auth_mode" -> "client_auth",
   "x_auth_password" -> password, "x_auth_username" -> username},
 "DisplayProxyDialog" -> False,"VerifyPeer"->False],FEConnectFail[1600];Throw[$Failed,$tag],
 	{MessageName[Utilities`URLTools`FetchURL, "conopen"], 
      MessageName[Utilities`URLTools`FetchURL, "contime"], 
      MessageName[Utilities`URLTools`FetchURL, "erropts"], 
      MessageName[Utilities`URLTools`FetchURL, "httperr"], 
      MessageName[Utilities`URLTools`FetchURL, "nofile"], 
      MessageName[Utilities`URLTools`FetchURL, "nolib"], 
      MessageName[URLFetch, "invhttp"],
      MessageName[General, "offline"], 
      MessageName[General, "nffil"]}];
 If[MatchQ[status,{_,_}],
 	{status,contentData}=status, 
 	FEConnectFail[1600];Throw[$Failed,$tag]
 ];
 content = fcc[contentData];
 Switch[status,
 	200,
 	content=Quiet[Check[(*TODO: handle other errors here*)
 		ImportString[content, "JSON"],Message[CloudConnect::apkey];$Failed,
 			{Import::fmterr}],{Import::fmterr}];
 	If[Not[MatchQ[content,{_Rule..}]],Return[$Failed]];
    log["Authentication response content: `1`", content, DebugLevel->3];
    setAuthentication[username, "uuid" /. content,
 		StringJoin[{"firstname"," ","lastname"}/. content],
 		"oauth_token_secret" /. content,
 		"oauth_token" /. content],

 	401,
 	check401AndIssueMessage[content];FEConnectFail[status];
    log["Authentication failed: `1`", {status, content}];
 	$Failed,

 	400,
 	Message[CloudConnect::badts];FEConnectFail[status];
    log["Authentication failed: `1`", {status, content}];
 	$Failed,

 	_,(*all other*)
 	Message[CloudConnect::oauth];FEConnectFail[status];
    log["Authentication failed: `1`", {status, content}];
 	$Failed
 	]],
     $tag]

doAuthenticatedURLFetch[func_, url_String, param_, opts___?OptionQ] := Catch[
    If[Not[TrueQ[$CloudConnected]],Throw[$Failed["MustAuthenticate"],$tag]];
    (* Do not remove the quiet, it will send messages if users are using deprecated options in URLFetch *)
    With[{method=Quiet[OptionValue[URLFetch, {opts}, "Method"], {OptionValue::nodef}], 
        headers=Quiet[OptionValue[URLFetch, {opts}, "Headers"], {OptionValue::nodef}],
        options=Sequence @@ FilterRules[{opts}, Except["Method"|"Headers"]]},
        With[{auth=makeOAuthHeader[url,method]},
            log["Headers: `1`", Join[headers, {"Authorization" -> auth}], DebugLevel->3];
            log["`1` `2`", method, url, DebugLevel->3];
            log["Options: `1`", ToString[{options}, InputForm], DebugLevel->3];
            Check[
                func[url, param, "Method"->method,
                    "Headers" ->Join[headers, {"Authorization" -> auth}],
                    options],
                Throw[$Failed,$tag]]]],
    $tag]

authenticatedURLFetch[url_String, elements:Except[_Rule], opts___?OptionQ] :=
    doAuthenticatedURLFetch[URLFetch, url, elements, opts]
    
authenticatedURLFetch[url_String, opts___?OptionQ] :=
    doAuthenticatedURLFetch[URLFetch, url, "Content", opts]

authenticatedURLFetchAsynchronous[url_String, callback_, opts___?OptionQ] :=
    doAuthenticatedURLFetch[URLFetchAsynchronous, url, callback, opts]

authenticatedQ[]:=TrueQ[$CloudConnected]

setAuthentication[username_String, uuid_String, userdisplayname_String, accesssecret_String, accesstoken_String] := If[TrueQ[$UseLibraryStorage],
	setWolframID[username]; setWolframUUID[uuid];
	setRegisteredUserName[userdisplayname];setAccessData[accesstoken,accesssecret];
	setCloudConnected[True]]
setAuthentication[___] := $Failed

getAuthentication[] := If[ TrueQ[$CloudConnected],
	{$WolframID,$WolframUUID,$RegisteredUserName,getAccessSecret[],getAccessToken[]},
	$Failed
]
getAuthentication[___] := $Failed

loadLibCloudObj[] :=
    Block[{path},

        path = FindLibrary["WolframAuthenticate"];
        If[path === $Failed,
        	path = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],
        		"LibraryResources", $SystemID, "WolframAuthenticate"}]
        ];
        AuthenticateLibraryFile = path;
        (
        	getConsumerKey = LibraryFunctionLoad[path, "get_consumer_key", {}, "UTF8String"];
            setAccessData = LibraryFunctionLoad[path, "set_access_data", {"UTF8String", "UTF8String"}, Integer];
            getAccessToken = LibraryFunctionLoad[path, "get_access_key", {}, "UTF8String"];
            getAccessSecret = LibraryFunctionLoad[path, "get_access_secret", {}, "UTF8String"];(*TODO: remove this?*)
            CloudHMAC = LibraryFunctionLoad[path,"cloud_object_oauth_hmac",{"UTF8String","UTF8String"},"UTF8String"];
			True/;SameQ[LibraryFunction,Sequence@@(Head/@{getConsumerKey,setAccessData,getAccessToken,getAccessSecret,CloudHMAC})]
        ) /; (path =!= $Failed)
    ]

loadLibCloudObj[___] := $Failed

$UseLibraryStorage = If[$CloudEvaluation === True,False,UnsameQ[loadLibCloudObj[], $Failed]]


$credsDir = FileNameJoin[{$UserBaseDirectory,"ApplicationData","CloudObject","Authentication"}];
$credsFile ="config.pfx";(*TODO:make this an actual pfx file*)
$storageKey:=Internal`HouseKeep[$credsDir, {
	"machine_ID" -> $MachineID,
	"version" -> $Version,
	"system_ID" -> $SystemID,
	"user_name" -> $UserName
}]

initencrypt[] := Symbol["NumberTheory`AESDump`RijndaelDecryption"][]

encrypt[args___]:=With[{ef=(initencrypt[];Symbol["NumberTheory`AESDump`Private`rijndaelEncryption"])},
	ef[args]
]

decrypt[args___]:= With[{df=(initencrypt[];Symbol["NumberTheory`AESDump`RijndaelDecryption"])},
	Block[{DataPaclets`SocialMediaDataDump`Private`flagQ = True, NumberTheory`AESDump`Private`flagQ = True},
	df[args]
	]
]

makeCredentialsChain[] := StringJoin[
	"cloudbase=",$CloudBase,
	"token=",getAccessToken[],
	"secret=",getAccessSecret[],
	"username=",$RegisteredUserName,
	"uuid=",$WolframUUID,
	"wolframid=",$WolframID
]

addToKeyChain[keychain_String,cloudbase_String] := Module[{chain=makeCredentialsChain[]},
	StringJoin[
		Riffle[
			Prepend[
				DeleteCases[
					StringSplit[keychain,"cloudbase="],
					x_String/;StringMatchQ[x,cloudbase~~__]
				],
			chain],
		"cloudbase="]
	]
]

storeCredentials[]:=storeCredentials[$credsDir,$credsFile,$storageKey]
storeCredentials[directory_String,filename_String,key_String]/;authenticatedQ[]:= Catch[
	Block[{$KeyChain=encrypt[addToKeyChain[getKeyChain[directory,filename,key],$CloudBase],key(*,"CiphertextFormat" -> "ByteList"*)]},
		With[{
			CreateDirectorymessages:={CreateDirectory::filex,CreateDirectory::privv},
			Savemessages:={Save::wtype,Save::argm,Save::argmu,General::stream,Save::sym,General::privv,General::noopen},
			file=FileNameJoin[{directory,filename}]
		},
		If[Not[DirectoryQ[directory]],
			Quiet[Check[CreateDirectory[directory],Throw[$Failed["NoMakeDir"],$tag],CreateDirectorymessages],CreateDirectorymessages]
		];
		Quiet[Check[DumpSave[file,$KeyChain],Throw[$Failed["NoDump"],$tag],Savemessages],Savemessages];
		True
		]
	],$tag]

storeCredentials[___]:=$Failed["NotAuthenticated"]

getKeyChain[] := getKeyChain[$credsDir, $credsFile, $storageKey]
getKeyChain[directory_String,filename_String,key_String] := ReplaceAll[Catch[
	If[TrueQ[$CloudDebug],Identity,Quiet][
	Block[{$KeyChain=""},With[
	{Getmessages:={Get::enkey,Get::notencode,General::privv,General::noopen},file=FileNameJoin[{directory,filename}]},
	If[Not[DirectoryQ[directory]],Return[""]];
	Quiet[Check[Get[file],Return[""],Getmessages],Getmessages];
	If[Not[MatchQ[$KeyChain,{_Integer..}]],Throw[$Failed["Bytes"],$tag]];
	$KeyChain=decrypt[$KeyChain, key];
	If[Not[StringQ[$KeyChain]],$KeyChain=ExportString[$KeyChain,"Byte"]];
	$KeyChain
]]],$tag],_$Failed->""]

getCredentials[]:=getCredentials[$credsDir,$credsFile,$storageKey]
getCredentials[directory_String,filename_String,key_String]:=Catch[
	If[TrueQ[$CloudDebug],Identity,Quiet][
	Block[{$KeyChain},
		$KeyChain=getKeyChain[directory,filename,key];
		If[!StringQ[$KeyChain],Throw[$Failed["NotString"],$tag]];
		$KeyChain = StringSplit[$KeyChain,"cloudbase="];
		$KeyChain = Cases[$KeyChain,x_String/;StringMatchQ[x,$CloudBase~~__]];
		If[!MatchQ[$KeyChain,{_String}],Throw[$Failed["NoCloudBase"],$tag],$KeyChain = First[$KeyChain]];
		$KeyChain = Rest[StringSplit[$KeyChain,{"token=", "secret=", "username=", "uuid=", "wolframid="}]];
		If[MatchQ[$KeyChain,{_String,_String,_String,_String,_String}],
		setAuthentication[Sequence@@Reverse[$KeyChain]],
		Throw[$Failed["NotStringPair"],$tag]
	]
]],$tag]
getCredentials[__]:=$Failed["BadParameters"]
(*
SetAttributes[{encrypt,decrypt,storeCredentials,
	getCredentials, $storageKey, setAuthentication, authenticateWithServer,
	authenticate,iCloudConnect, flushCredentials}, Locked];
*)
SetAttributes[CloudConnect,ReadProtected];
Protect[CloudConnect];
SetAttributes[CloudDisconnect,ReadProtected];
Protect[CloudDisconnect];
Protect[$CloudConnected];

End[]

EndPackage[]
