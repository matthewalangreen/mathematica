(* Mathematica package *)
BeginPackage["CloudObject`"];

Begin["`Private`"];

wtf2[] := Module[{ceStr2 = "$CloudEvaluation"},
	Hold[Evaluate[CloudEvaluate[ToExpression[ceStr2]]]]
];


End[]

EndPackage[]
