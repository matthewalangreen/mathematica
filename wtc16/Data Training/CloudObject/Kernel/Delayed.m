(* Mathematica package *)
BeginPackage["CloudObject`"]

System`Delayed;

Begin["`Private`"]

Unprotect[Delayed];

Options[Delayed] = {UpdateInterval -> Infinity};
Attributes[Delayed] = {HoldFirst};

Delayed[expr_, ___][___] := expr

SetAttributes[Delayed, {ReadProtected}];
Protect[Delayed]

End[]

EndPackage[]
