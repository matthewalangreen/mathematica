(* ::Package:: *)

(* General utilities *)
Echo[x_]:= (Print[x];x)
(* Associaction temp code, remove this with a stable release of math 10 *)

ClearAll[ToCollection, JoinCollection]

JoinCollection[a__] := Collection[Join @@ List @@@ Flatten[{a}]]

ToCollection[any_Collection] := any;
ToCollection[{rules___Rule}] := Collection[{rules}];
ToCollection[{a_, b_}] := Collection[{a->b}]

(* HTML Escape symbol *)

ClearAll[HTMLEscape]

HTMLEscape::usage = "EscapeHTML removes dangerous character inside a string: &<>\'\"";
HTMLEscape[s_String] := StringReplace[s,{
	"&" -> "&amp;",
	"<" -> "&lt;",
	">" -> "&gt;",
	"\"" -> "&quot;",
	"'" -> "&#39;"
}]
ClearAll[
	$HttpStatusCodes, 
	$HttpDefaultStatusCode,
	HttpResponse,
	HttpRedirect,
	HttpRedirectPermanent,  
	HttpRedirectContent,
	HttpPermanentRedirect, 
	HttpStatusDescription, 
	HttpResponseRenderHead, 
	HttpResponseRenderContent, 
	HttpCookie, 
	HttpHeader
]

(* HttpResponse global defaults, use them with Block *)

$HttpDefaultStatusCode::usage = "Default status code for HttpResponse";
$HttpDefaultStatusCode = 200;

$HttpDefaultHeaders::usage = "Default headers for HttpResponse";
$HttpDefaultHeaders = {
	{"ContentType" -> "text/html; charset=" <> ToLowerCase[$CharacterEncoding]}
};

$HttpStatusCodes::usage = "Hypertext Transfer Protocol (HTTP) response status codes. 
This includes codes from IETF internet standards as well as other IETF RFCs, other specifications and some additional commonly used codes. 
The first digit of the status code specifies one of five classes of response; For more information visit http://en.wikipedia.org/wiki/List_of_HTTP_status_codes";
$HttpStatusCodes = Collection[{
    100 -> "CONTINUE",
    101 -> "SWITCHING PROTOCOLS",
    102 -> "PROCESSING",
    200 -> "OK",
    201 -> "CREATED",
    202 -> "ACCEPTED",
    203 -> "NON-AUTHORITATIVE INFORMATION",
    204 -> "NO CONTENT",
    205 -> "RESET CONTENT",
    206 -> "PARTIAL CONTENT",
    207 -> "MULTI-STATUS",
    208 -> "ALREADY REPORTED",
    226 -> "IM USED",
    300 -> "MULTIPLE CHOICES",
    301 -> "MOVED PERMANENTLY",
    302 -> "FOUND",
    303 -> "SEE OTHER",
    304 -> "NOT MODIFIED",
    305 -> "USE PROXY",
    306 -> "RESERVED",
    307 -> "TEMPORARY REDIRECT",
    400 -> "BAD REQUEST",
    401 -> "UNAUTHORIZED",
    402 -> "PAYMENT REQUIRED",
    403 -> "FORBIDDEN",
    404 -> "NOT FOUND",
    405 -> "METHOD NOT ALLOWED",
    406 -> "NOT ACCEPTABLE",
    407 -> "PROXY AUTHENTICATION REQUIRED",
    408 -> "REQUEST TIMEOUT",
    409 -> "CONFLICT",
    410 -> "GONE",
    411 -> "LENGTH REQUIRED",
    412 -> "PRECONDITION FAILED",
    413 -> "REQUEST ENTITY TOO LARGE",
    414 -> "REQUEST-URI TOO LONG",
    415 -> "UNSUPPORTED MEDIA TYPE",
    416 -> "REQUESTED RANGE NOT SATISFIABLE",
    417 -> "EXPECTATION FAILED",
    422 -> "UNPROCESSABLE ENTITY",
    423 -> "LOCKED",
    424 -> "FAILED DEPENDENCY",
    426 -> "UPGRADE REQUIRED",
    500 -> "INTERNAL SERVER ERROR",
    501 -> "NOT IMPLEMENTED",
    502 -> "BAD GATEWAY",
    503 -> "SERVICE UNAVAILABLE",
    504 -> "GATEWAY TIMEOUT",
    505 -> "HTTP VERSION NOT SUPPORTED",
    506 -> "VARIANT ALSO NEGOTIATES",
    507 -> "INSUFFICIENT STORAGE",
    508 -> "LOOP DETECTED",
    510 -> "NOT EXTENDED"
}];
(* Symbolic rappresentation of Header and cookies *)

HttpHeader::usage = "Symbolic rappresentation of an HTTP Header. Backward compatibility for URLFetch syntax.";
HttpHeader[{a_, b_}] := HttpHeader[a -> b]
HttpHeader /: ToString[HttpHeader[rule_Rule]] := StringJoin[ToString[rule[[1]]], ": ", ToString[rule[[2]]]]
HttpHeader /: ToString[HttpHeader[___]] := $Failed;

HttpCookie::usage = "Symbolic rappresentation of an HTTP Cookie. Backward compatibility for URLFetch syntax.";
HttpCookie[{rules___Rule}] := HttpCookie[Collection[{rules}]]
HttpCookie /: ToString[HttpCookie[data_Collection]] := StringJoin[Riffle[Map[Riffle[#, "="] &, List @@@ (List @@ data)], "; "] /. {x_String :> x, x_List :> x, x_ :> ToString[x]}]
HttpCookie /: ToString[HttpCookie[___]] := $Failed;
(* utility to extract description from Collection *)

HttpStatusDescription::usage = "Utility to return a short description of Hypertext Transfer Protocol (HTTP) response status codes.
Accept integer or string as first parameter, default description/fallback as second parameter.";
HttpStatusDescription[x_Integer, default_String: "UNKNOWN STATUS CODE"] := With[
	{res = $HttpStatusCodes[x]}, 
	If[StringQ[res], res, default]
]
HttpStatusDescription[x_String, args___] /; StringMatchQ[x, DigitCharacter..] := HttpStatusDescription[ToExpression[x], args]
HttpStatusDescription[_, default_String: "UNKNOWN STATUS CODE"] := default

HttpResponseRenderHead::usage = "Utility function to render just the HEAD of an HTTP Response";
HttpResponseRenderHead[OptionsPattern[]] := StringJoin[
	"HTTP/1.0 ", ToString[OptionValue["StatusCode"]], " ", HttpStatusDescription[OptionValue["StatusCode"]], "\n",
	Riffle[
		Join[
			(*Map[ToString[HttpHeader[#]] &, Echo[List @@ JoinCollection[ToCollection[$HttpDefaultHeaders], ToCollection /@ OptionValue["Headers"]]]], *)
			Map[ToString[HttpHeader[#]] &, List @@ JoinCollection[Join[ToCollection /@ $HttpDefaultHeaders, ToCollection /@ OptionValue["Headers"]]]],
			Map[ToString[HttpHeader["SetCookie" -> ToString[HttpCookie[#]]]] &, List @@ Map[ToCollection, OptionValue["Cookies"]]]
		],
		"\n"
	]
];

Options[HttpResponseRenderHead] := {
	"Headers" -> {}, 
	"Cookies" -> {}, 
	"StatusCode" -> $HttpDefaultStatusCode
};

HttpResponseRenderContent::usage = "Utility to render the content of an HTTP Response. Accept a string or a callable.";
HttpResponseRenderContent[content_String] := content
HttpResponseRenderContent[content_] := content[]

HttpResponse::usage = "Rapresents an HTTP Response. Allowed options: \"StatusCode\", \"Headers\" and \"Cookies\". Backward compatibility with URLFetch syntax.";
HttpResponse /: ToString[HttpResponse[content_, options___]] := With[{
	rendered = HttpResponseRenderContent[content]
	}, 
	If[
		StringLength[rendered] > 0,
		StringJoin[{HttpResponseRenderHead[options], "\n\n", rendered}],
		HttpResponseRenderHead[options]
	]
]

HttpRedirectContent::usage = "Change the HTML Fallback for an HttpRedirect or HttpRedirectPermanent.";
HttpRedirectContent[url_] := "<html><head><meta http-equiv=\"Refresh\" content=\"0; url="<>HTMLEscape[url]<>"\" /></head><body></body></html>";

HttpRedirect::usage = "Rappresents an Http Redirect, status code of 300. Use this for dynamic redirects.";
HttpRedirect /: ToString[HttpRedirect[url_String, options___]] := ToString[
	Block[
		{$HttpDefaultHeaders = Join[$HttpDefaultHeaders, {{"Location", url}}], $HttpDefaultStatusCode = 300},
		ToString[HttpResponse[
			HttpRedirectContent[url],
			options
		]]
	]
]

HttpRedirectPermanent::usage = "Rappresents an Http Permanent Redirect, status code of 301. 
Use this to indicate that a resource has been moved permanently.
Useful for SEO optimization, cached by modern browsers.";
HttpRedirectPermanent /: ToString[HttpRedirectPermanent[url_String, options___]] := ToString[
	Block[
		{$HttpDefaultHeaders = Join[$HttpDefaultHeaders, {{"Location", url}}], $HttpDefaultStatusCode = 301},
		ToString[HttpResponse[
			HttpRedirectContent[url],
			options
		]]
	]
]
(* Sample Usage Headers and Cookies can be collections, list of rules of list of couples *)

response = HttpResponse[
	"Hello world!", 
	"Headers" -> {
		{"Date","Fri, 20 Jan 2012 19:14:33 GMT"},
		{"Server","Apache"},
		{"Content-Language","en"},
		{"Vary","Accept-Language"},
		{"Content-Location","/index.en.html"},
		{"Connection","close"},
		{"Transfer-Encoding","chunked"}
	}, 
	"Cookies" -> {
		{"Domain"->".wolfram.com", "Path"->"/", "Secure"->"FALSE", "Expires"->"Mon 17 Jan 2022 19:13:50", "Name"->"WR_SID", "Value"->"8cb1c8ef2c064f19bceeb8d7"},
		{"Domain"->"login.wolfram.com", "Path"->"/", "Secure"->"TRUE", "Expires"->"Mon 17 Jan 2022 19:13:50", "Name"->"WR_LOGIN", "Value"->"23748912837413hhrkfjdhfadj"}
	},
	"StatusCode" -> 200
];

ToString[response]

response = HttpRedirect["/path/to/redirect"]
ToString[response]


(* ::Print:: *)
(*JoinCollection[Collection[Join[List@@$HttpDefaultHeaders],{{"Location","/path/to/redirect"}}],{}]*)


(* ::Print:: *)
(*JoinCollection[<["ContentType"->"text/html; charset=utf-8"]>,{<["Date"->"Fri, 20 Jan 2012 19:14:33 GMT"]>,<["Server"->"Apache"]>,<["Content-Language"->"en"]>,<["Vary"->"Accept-Language"]>,<["Content-Location"->"/index.en.html"]>,<["Connection"->"close"]>,<["Transfer-Encoding"->"chunked"]>}]*)


(* ::Print:: *)
(*JoinCollection[<["ContentType"->"text/html; charset=utf-8"]>,{ToCollection[{"Date","Fri, 20 Jan 2012 19:14:33 GMT"}],ToCollection[{"Server","Apache"}],ToCollection[{"Content-Language","en"}],ToCollection[{"Vary","Accept-Language"}],ToCollection[{"Content-Location","/index.en.html"}],ToCollection[{"Connection","close"}],ToCollection[{"Transfer-Encoding","chunked"}]}]*)


(* ::Print:: *)
(*Collection[Join@@Apply[List,{Collection[Join[List@@$HttpDefaultHeaders],{{"Location","/path/to/redirect"}}],<[]>},{1}]]*)


(* ::Print:: *)
(*JoinCollection[<["ContentType"->"text/html; charset=utf-8"]>,ToCollection[{{"Date","Fri, 20 Jan 2012 19:14:33 GMT"},{"Server","Apache"},{"Content-Language","en"},{"Vary","Accept-Language"},{"Content-Location","/index.en.html"},{"Connection","close"},{"Transfer-Encoding","chunked"}}]]*)


(* ::Print:: *)
(*JoinCollection[ToCollection[<["ContentType"->"text/html; charset=utf-8"]>],ToCollection[{{"Date","Fri, 20 Jan 2012 19:14:33 GMT"},{"Server","Apache"},{"Content-Language","en"},{"Vary","Accept-Language"},{"Content-Location","/index.en.html"},{"Connection","close"},{"Transfer-Encoding","chunked"}}]]*)


(* ::Print:: *)
(*JoinCollection[ToCollection[Collection[Join[List@@$HttpDefaultHeaders],{{"Location","/path/to/redirect"}}]],<[]>]*)



