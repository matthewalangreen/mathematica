BeginPackage["CloudObject`"]

System`ExportForm;
System`ResponseForm;
System`UpdateInterval;
System`CloudExport;
System`CloudImport;

Begin["`Private`"]

Unprotect[CloudObject];

(* Formats *)

mimeToFormat =
    Quiet[DeleteCases[Flatten @ Map[
        Function[{format}, Function[{mime}, mime -> format] /@ ImportExport`GetMIMEType[format]], $ExportFormats],
        $Failed], FileFormat::fmterr];

(* Give non-"application/..." types precedence (e.g. image/png should be used instead of application/png).
 But prefer application/pdf over text/pdf. *)
uniqueType[types_List] := First @ SortBy[ToLowerCase /@ types, If[StringMatchQ[#, "application" ~~ __] && # =!= "application/pdf", 2, 1] &]
uniqueType[type_] := uniqueType[{type}]

formatToMime =
    Quiet[Map[# -> (If[Length[#] > 0, uniqueType @ #, "application/octet-stream"] &) @
        ImportExport`GetMIMEType[#] &, $ExportFormats
    ], FileFormat::fmterr];

mimetypeToFormat[type_, filename_: Null] := type /. Join[mimeToFormat, {
    _ -> If[filename =!= Null, FileFormat[filename], "Text"]
}]

formatToMimeType[format_] := format /. Join[
    {
        "HTMLFragment" -> "text/html",
        "NBElement" -> "application/vnd.wolfram.notebook.element",
        "CloudCDF" -> "application/vnd.wolfram.notebook",
        "JPG" -> "image/jpeg",  (* ImportExport`GetMIMEType lacks definition for "JPG", so add it here *)

        "API" -> "application/vnd.wolfram.expression.api",
        "Computation" -> "application/vnd.wolfram.expression.computation",
        "Form" -> "application/vnd.wolfram.expression.form",
        "Task" -> "application/vnd.wolfram.expression.task",
        "Grammar" -> "application/vnd.wolfram.expression.grammar",
        "Expression" -> "application/vnd.wolfram.expression"
    },
    formatToMime,
    {
        _ -> "application/octet-stream"
    }
]
formatToMimeType[mime_ ? (StringMatchQ[#, __ ~~ "/" ~~ __]&)] := mime

exportFormat["NBElement"] = "NB";
exportFormat["CloudCDF"] = "NB";
exportFormat["API"] = exportFormat["Computation"] = exportFormat["Form"] = exportFormat["Task"] = exportFormat["Grammar"] = "Expression";
exportFormat[format_] := format

(* ExportForm *)

Unprotect[ExportForm];

toNotebook[expr : (_Notebook | _NotebookObject | _DocumentNotebook | _PaletteNotebook | _DialogNotebook)] := expr
toNotebook[expr_, cellopts___] := Notebook[{
	If[Head[expr] === Cell,
		Cell[First[expr], expr[[2]], cellopts],
	(* not a cell *)
	    Cell[BoxData[ToBoxes[expr]], "Output", cellopts]
	]
}]
toNotebookElement[expr_] := toNotebook[expr, TextAlignment->Center, ShowCellBracket->False]

exportAsNotebookElement[_Manipulate|_Graphics|_Graphics3D] = True;
exportAsNotebookElement[_] = False;

(* one-argument short forms, also used by CloudDeploy so it automatically chooses a proper format *)
ExportForm[expr_NotebookObject, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]
ExportForm[expr_Notebook, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]
ExportForm[expr_DocumentNotebook, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]
ExportForm[expr_PaletteNotebook, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]
ExportForm[expr_DialogNotebook, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]
ExportForm[expr_Image, opts:OptionsPattern[]] := ExportForm[expr, "PNG", opts]
ExportForm[expr_Sound, opts:OptionsPattern[]] := ExportForm[expr, "MP3", opts]
ExportForm[expr_?exportAsNotebookElement, opts:OptionsPattern[]] := ExportForm[expr, "NBElement", opts]

(* By default, export as a notebook. Export will take care of wrapping in a Notebook expression. *)
ExportForm[expr_, opts:OptionsPattern[]] := ExportForm[expr, "HTMLCloudCDF", opts]

SetAttributes[ExportForm, {ReadProtected}];
Protect[ExportForm];

(*Import*)

Unprotect[CloudImport];

CloudImport[obj_CloudObject, format_ : Automatic] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = readObject[obj];
        If[tempfilename === $Failed, Return[$Failed]];
        cleanup[tempfilename,
            Import[tempfilename,
                If[format === Automatic, mimetypeToFormat[mimetype, tempfilename], format]
            ]
        ]
    ]

CloudImport[uri_String, format_ : Automatic] := CloudImport[CloudObject[uri], format]

CloudObject /: Import[obj_CloudObject, format_ : Automatic] := CloudImport[obj, format]

SetAttributes[CloudImport, {ReadProtected}];
Protect[CloudImport];

(*Export*)

Unprotect[CloudExport];

Options[CloudExport] = {Permissions->Automatic, IconRules->Automatic, MetaInformation->{}};

wrapExportExpr[expr_Manipulate, "NBElement", opts:OptionsPattern[ExportForm]] :=
    toNotebookElement[Append[expr, SaveDefinitions->True]]
wrapExportExpr[expr_, "NBElement", opts:OptionsPattern[ExportForm]] :=
    toNotebookElement[expr]
wrapExportExpr[expr_, "CloudCDF", opts:OptionsPattern[ExportForm]] :=
    toNotebook[expr]
wrapExportExpr[expr_, form_, opts:OptionsPattern[ExportForm]] := Unevaluated[expr]
Attributes[wrapExportExpr] = {HoldFirst};

interactiveBoxesQ[_DynamicBox | _Graphics3DBox | _Dynamic | _DynamicModuleBox] = True;
interactiveBoxesQ[head_[args___]] := interactiveBoxesQ[head] || ReleaseHold[Map[interactiveBoxesQ, Hold[Or[args]], {2}]]
interactiveBoxesQ[other_] = False;
Attributes[interactiveBoxesQ] = {HoldAllComplete};

applyExportForm[expr_, format_, rest___] := 
	Module[{str, type},
		str = ExportString[wrapExportExpr[expr, format], exportFormat[format], rest];
		type = formatToMimeType[format];
		If[str === $Failed, {str, type}, {ToCharacterCode[str], type} ]			
	]
applyExportForm[expr_, "HTMLCloudCDF", rest___] :=
    Module[{boxes},
        If[ReleaseHold[Hold[interactiveBoxesQ[boxes]] /. boxes -> ToBoxes[expr, StandardForm]],
            applyExportForm[expr, "CloudCDF", rest],
            {First[applyExportForm[expr, "HTMLFragment", rest]], "application/vnd.wolfram.cloudcdf.html"}
        ]
    ]
applyExportForm[expr_, "HTML", rest___] :=
    applyExportForm[expr, "HTMLFragment", rest, "FullDocument" -> True]
applyExportForm[expr_String, "HTML", rest___] :=
    applyExportForm[expr, "HTMLFragment", rest]    
applyExportForm[expr:Alternatives[Sound[_SoundNote], Sound[{__SoundNote}]], "MP3", rest___]:=
	applyExportForm[Sound`ToSampledSound[expr], "MP3", rest]	   
Attributes[applyExportForm] = {HoldFirst};

CloudExport[expr_, format_, obj:CloudObject[uri_, objopts:OptionsPattern[CloudObject]], rest___Rule] :=
    Module[{content, mimetype, permissions, result},
        {content, mimetype} = applyExportForm[expr, format, rest];
        If[content === $Failed,Return[$Failed]];
        permissions = Quiet[OptionValue[CloudExport, {rest, objopts}, Permissions], OptionValue::nodef];
        writeObject[obj, content, mimetype, permissions,
            Quiet[OptionValue[CloudExport, {rest, objopts}, IconRules], OptionValue::nodef],
            Unevaluated[expr],
            Quiet[OptionValue[CloudExport, {rest, objopts}, MetaInformation], OptionValue::nodef],
            CloudExport]
    ]

CloudExport[expr_, format_, uri_String, rest___Rule] :=
    CloudExport[Unevaluated[expr], format, CloudObject[uri], rest]

CloudExport[expr_, format_, rest___Rule] :=
    CloudExport[Unevaluated[expr], format, CloudObject[], rest]

CloudExport[args___] := (ArgumentCountQ[CloudExport, Length[DeleteCases[{args},_Rule,Infinity]],2,3];Null/;False)

CloudObject /: Export[obj_CloudObject, expr_, format_, rest___] :=
    CloudExport[Unevaluated[expr], format, obj, rest]

SetAttributes[CloudExport, {ReadProtected}];
Protect[CloudExport];

Protect[CloudObject];

End[]

EndPackage[]
