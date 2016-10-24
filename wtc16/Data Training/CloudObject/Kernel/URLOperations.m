BeginPackage["CloudObject`"]

Begin["`Private`"]

(* URLFetch *)

Unprotect[CloudObject];
CloudObject /: URLFetch[CloudObject[url_, ___], opts:OptionsPattern[]] :=
    If[TrueQ[$CloudEvaluation] || !TrueQ[$CloudConnected], URLFetch, authenticatedURLFetch][url, opts]

CloudObject /: URLFetch[CloudObject[url_, ___], arg_, opts:OptionsPattern[]] :=
    If[TrueQ[$CloudEvaluation] || !TrueQ[$CloudConnected], URLFetch, authenticatedURLFetch][url, arg, opts]

CloudObject /: URLFetch[CloudObject[url_, ___], args__, opts:OptionsPattern[]] := (
	Message[URLFetch::argb, URLFetch, 1 + Length[{args}],0,1]; $Failed
)

Protect[CloudObject];

End[]

EndPackage[]
