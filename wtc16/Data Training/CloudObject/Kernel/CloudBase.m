BeginPackage["CloudObject`"]

System`$CloudBase;

Begin["`Private`"]

If[!StringQ[$UrlScheme],
    $UrlScheme = "https";
];
$CloudBase = "https://www.wolframcloud.com/";
If[ValueQ[CloudSystem`$ApplicationDomain],
    If[CloudSystem`$ApplicationDomain === "localhost" || CloudSystem`$ApplicationDomain === "localhost:8080",
        $UrlScheme = "http";
    ];
    $CloudBase = $UrlScheme<>"://" <> CloudSystem`$ApplicationDomain <> "/";
];

End[]

EndPackage[]
