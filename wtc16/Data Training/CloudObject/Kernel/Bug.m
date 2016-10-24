(* Mathematica package *)
BeginPackage["CloudObject`"];

TestPublic::usage = "";
TestPublic2::usage = "";

Begin["`Private`"];

b = "bar";
TestPublic[a_] := Module[
	{name2 = a},
	
    (*Global`myExprToStringBytesWithSaveDefinitions[*)
    Language`ExtendedFullDefinition[
		(Characters[name2] &)
	]
]

TestPublic2[a_] := With[
	{name = a},
	
    Global`myExprToStringBytesWithSaveDefinitions[
    (*Language`ExtendedFullDefinition[*)
		(Characters[Global`baz] &)
	]
]

End[]

EndPackage[]
