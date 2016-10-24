BeginPackage["CloudObject`"]

System`CloudEvaluate;
System`CloudFunction;

Begin["`Private`"]

getAPIResult[obj_CloudObject, arguments_ : {}] :=
    Module[{body, result},
        body = StringJoin[#1 <> "=" <> #2& @@@ arguments];
        (* TODO: Remove view parameter in favor of _view when that change is deployed to production. *)
        result = responseToExpr @ execute[obj, "POST", "objects",
            Parameters -> {"view" -> "API", "_view" -> "API", "exportform" -> "WL", "responseform" -> "WL"},
            Body -> body, Type -> "application/x-www-form-urlencoded"
        ];
        ReleaseHold /@ result["MessagesExpressions"];
        If[result === $Failed, Return[$Failed]];
        If[KeyExistsQ[result, "Result"], ToExpression[result["Result"]], Null]
    ]

(* CloudFunction *)

Unprotect[CloudFunction];

If[TrueQ[$CloudEvaluation],
	CloudFunction[fn_][args___] := fn[args]
	,
(* Else calling from outside the cloud *)
	(* CloudFunction stores an expression as an APIFunction in the cloud and executes it (in the cloud). *)
	CloudFunction[expr_][args___] :=
	    Module[{co},
	        Block[{formalargs},
	            co = CloudPut[APIFunction[{"args" -> "String"},
	                ResponseForm[ExportForm[expr @@ ToExpression[#args], "WL"], "WL"] &
	            ], SaveDefinitions -> True, IconRules -> {}];
	            If[co === $Failed, Return[$Failed]];
	            (* TODO: This cleanup could happen asynchronously. *)
	            cleanup[co, getAPIResult[co, {"args" -> URLEncode[ToString[{args}, InputForm]]}]]
	        ]
	    ];
	
]

CloudFunction[obj_CloudObject][args___] := CloudFunction[Get[obj]][args]
	
CloudFunction[args___] := (ArgumentCountQ[CloudFunction,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

SetAttributes[CloudFunction, {ReadProtected}];
Protect[CloudFunction];

(*CloudEvaluate*)

Unprotect[CloudEvaluate];

CloudEvaluate[expr_] := CloudFunction[expr &][]

CloudEvaluate[args___] := (ArgumentCountQ[CloudEvaluate,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)
Attributes[CloudEvaluate] = {HoldAll};
	
SetAttributes[CloudEvaluate, {ReadProtected}];
Protect[CloudEvaluate];

End[]

EndPackage[]
