(* ::Package:: *)

BeginPackage["CloudObject`"]

Begin["`Private`"]

loginDialog[username_String] := With[{messagePane = errorMessagePane[CurrentValue["WolframCloudLoginError"]], btnlblStyle = {"DialogStyle", "ControlStyle", FontSize -> (Inherited*0.95)}, boxid = "username", pwdboxid="passwd"},
  Block[{$loginCredentials}, 
   Module[{uname = username, pwd = "", leftCol, rightCol, columns}, 
   	Clear[$loginCredentials];
    If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
     leftCol = Column[{
        Column[{
          ExpressionCell[Row[{
             tr["WolframIDLabel1"](*"Wolfram ID "*)," ",
             Style[ tr["WolframIDLabel2"](*"(your email address)"*),FontSize -> (Inherited*0.85)]}], "DialogStyle","ControlStyle", FormatType -> TextForm],
             InputField[Dynamic[uname], String, ContinuousAction -> True,  System`BoxID -> boxid]
          }],
        Column[{
          TextCell[tr["PasswordLabel"](*"Password"*), "DialogStyle", "ControlStyle"],
          
          InputField[Dynamic[pwd], String, ContinuousAction -> True, 
           FieldMasked -> True, System`BoxID -> pwdboxid]
          }
         ],
		 DynamicWrapper[Grid[{
             {
			 EventHandler[
				PaneSelector[
					{
						False -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, Gray],
						True -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, RGBColor[0.878431,0.513725,0.133333]]
					},
					FrontEnd`CurrentValue["MouseOver"]
				],
				"MouseClicked" :> (FrontEndExecute[FrontEnd`NotebookLocate[{URL["https://user.wolfram.com/portal/password/forgot.html"], None}]])			
			 ]           
             },
             {
             Grid[{{
				Toggler[Dynamic[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]],
				{
					True -> CloudDialogImage["CheckboxOn"],
					False -> CloudDialogImage["CheckboxOff"]
				}],
				EventHandler[Style[tr["RemeberMeLabel"], "DialogStyle", "ControlStyle"],
					"MouseClicked" :> (
						FEPrivate`Set[
							FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}],
							FEPrivate`Not[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]]
						]
					)]
				}},
			 Alignment -> {Automatic, Center}]
             },
			{messagePane}, (*Login error message display*)
			{
			 Grid[{{
             Button[
              
              Pane[Style[tr["SignInButtonLabel" ](*"Sign In"*), btnlblStyle, White], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[$loginCredentials],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["SigninButton","Default"], 
              	"Hover" -> CloudDialogImage["SigninButton","Hover"], 
                "Pressed" -> CloudDialogImage["SigninButton","Pressed"],
				"ButtonType" -> "Default"}],

             Button[
              
              Pane[Style[tr["CancelButtonLabel" ](*"Sign In"*), btnlblStyle, RGBColor[0.266667,0.266667,0.266667]], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionCancelled[]; $loginCredentials = $Canceled],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["CancelButton","Default"], 
              	"Hover" -> CloudDialogImage["CancelButton","Hover"], 
                "Pressed" -> CloudDialogImage["CancelButton","Pressed"],
				"ButtonType" -> "Cancel"}]
			 }}]

			}},
		  Alignment -> {Left, Automatic},
		  Spacings -> {Automatic, 1}],
          $loginCredentials = {uname, pwd}]
        },
       Alignment -> Left,
       Spacings -> .5];
     rightCol = Pane[Column[{CloudDialogImage["CloudLogoIcon"],
        Style[tr["CloudAccountQuery"](*"Don't have a Wolfram Cloud account yet?"*), 
         "DialogStyle", "ControlStyle", FontSize -> (Inherited*0.9), LineSpacing -> {1, 0}],
		Button[
          
          Pane[Style[tr["JoinNowButtonLabel"](*"Join Now"*), btnlblStyle, White], 
           ImageMargins -> {{10, 10}, {0, 0}}],
          FE`hyperlinkCoded["http://www.wolfram.com/mathematica-cloud-access", "source=cloudlogindialog"],
          ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
          Appearance -> {
          	"Default" -> CloudDialogImage["JoinNowButton","Default"], 
          	"Hover" -> CloudDialogImage["JoinNowButton","Hover"],
          	"Pressed" -> CloudDialogImage["JoinNowButton","Pressed"]
          	}]
        },
       Alignment -> Center,
       ItemSize -> Scaled[.6],
       Spacings -> 2.5],
	 FrameMargins -> {{0, 0}, {0, 5}}];
     
     
     columns = Grid[{
        {"", Pane[
          leftCol,
          Full,
          Alignment -> Left
          ], Pane[
          rightCol,
          Full,
          Alignment -> Center
          ]}
       } ,
       Dividers -> {{None, {3 -> Directive[RGBColor[0.74, 0.74, 0.74]]}}, None},
       Spacings -> {0, 1},
       ItemSize -> {{Automatic, 
          1 -> FEPrivate`If[
            FEPrivate`SameQ[FEPrivate`$OperatingSystem, "MacOSX"], 4, 
            3]}, Automatic},
       Alignment -> {{Left, {-1 -> Center}}, Top}];
     
     DialogInput[
      ExpressionCell[
       Framed[
        Column[{Panel["", Appearance -> {"Default" -> CloudDialogImage["TopBanner"]}, ImageSize -> {Full, Automatic},
           FrameMargins -> {{10, 10}, {0, 0}}, 
           Alignment -> {Left, Center}],
          
          Panel[
           Pane[
            columns,
            {Full,All},
            ImageMargins -> {{0, 0}, {10, 15}}
            ],
           Appearance -> {"Default" -> CloudDialogImage["BackgroundImage"]}
           ]
          },
         Spacings -> {0, 0}
         ],
        ImageSize -> {Full, Full},
        FrameMargins -> 0,
        ImageMargins -> {{0, 0}, {-3, -1}},
        FrameStyle -> None
        ],
       CellMargins -> {{-1, -5}, {0, -2}},
       CellFrameMargins -> 0
       ],
      StyleDefinitions -> 
		Notebook[{
			Cell[StyleData[StyleDefinitions -> "Dialog.nb"]],
			Cell[StyleData["CloudConnectionErrorLink"],
				ButtonBoxOptions -> {
					Evaluator -> "System",
					ButtonFunction :> (FE`hyperlinkCoded[
						"http://www.wolfram.com/support/contact/email/", 
						"source=signinscreen"]),
					Appearance -> None},
				FontColor -> RGBColor[0, 0, 1]],
			Cell[StyleData["DesktopAccessErrorLink"],
				ButtonBoxOptions -> {
					Evaluator -> "System",
					ButtonFunction :> (FE`hyperlinkCoded[
						"https://www.wolframcloud.com/app/account/", 
						"source=signinscreen"]),
					Appearance -> None},
				FontColor -> RGBColor[0, 0, 1]]
		}, Visible -> False],
      WindowTitle -> FEPrivate`FrontEndResource["CloudLoginDialog", "WindowTitle"](*"Enter Login Credentials"*),
      WindowSize -> {520, FitAll}, 
      Modal -> True,
      NotebookDynamicExpression :> (
         Refresh[If[uname === "",
  			FrontEndExecute[
   				FrontEnd`MoveCursorToInputField[EvaluationNotebook[], boxid]
   			],
  			FrontEndExecute[
   				FrontEnd`MoveCursorToInputField[EvaluationNotebook[], pwdboxid]
   			]
  		], None]
 )
      ];
     $loginCredentials,
     (*Else,no interactive FE*)Return[$Canceled];]]
     ]
  ]
  
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*)   
(*----------------------------------------------------------------------------------------------------------------------------------*)   
(*----------------------------------------------------------------------------------------------------------------------------------*)  
(*---------------------------------------------------------------FE Resources Below Here--------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
  
tr[resource_String, id_] := Dynamic@RawBoxes@FEPrivate`FrontEndResource[resource, id]

tr[id_] := tr["CloudLoginDialog", id]

imgr[relPth_List, flNm_String] := FrontEnd`FileName[relPth, flNm]

imgr[flNm_String] := imgr[{"Dialogs", "CloudLogin"}, flNm]

imgimportr[relPth_List, flNm_String] := 
  Dynamic[RawBoxes@
    FEPrivate`ImportImage[FrontEnd`ToFileName[relPth, flNm]]]
    
imgimportr[flNm_String] := 
 imgimportr[{"Dialogs", "CloudLogin"}, flNm]

CloudDialogImage["CloudLogoIcon"] = imgimportr["CloudLogoIcon.png"];

CloudDialogImage["CheckboxOn"] = imgimportr["CheckboxOn.png"];

CloudDialogImage["CheckboxOff"] = imgimportr["CheckboxOff.png"];

CloudDialogImage["BackgroundImage"] = imgr["Background.9.png"];

CloudDialogImage["TopBanner"] = imgr["Banner.9.png"];

CloudDialogImage["SigninButton","Default"] = imgr["SigninButton-Default.9.png"];
                    
CloudDialogImage["SigninButton","Hover"] = imgr["SigninButton-Hover.9.png"];
                    
CloudDialogImage["SigninButton","Pressed"] = imgr["SigninButton-Pressed.9.png"];

CloudDialogImage["CancelButton","Default"] = imgr["CancelButton-Default.9.png"];
                    
CloudDialogImage["CancelButton","Hover"] = imgr["CancelButton-Hover.9.png"];
                    
CloudDialogImage["CancelButton","Pressed"] = imgr["CancelButton-Pressed.9.png"];

CloudDialogImage["JoinNowButton","Default"] = imgr["JoinNowButton-Default.9.png"];
                
CloudDialogImage["JoinNowButton","Hover"] = imgr["JoinNowButton-Hover.9.png"];
                
CloudDialogImage["JoinNowButton","Pressed"] = imgr["JoinNowButton-Pressed.9.png"];

errorMessagePane[showMessage_, errorMessage_] := PaneSelector[
	{
		True -> Pane[
				Style[errorMessage, LineIndent -> 0, RGBColor[0.9, 0.55, 0.32]], {Full, Full}, 
				Alignment -> {Left, Center}
			]
	},
	TrueQ[showMessage],
	ImageSize -> {220, 50},
	ImageMargins -> {{0, 0}, {5, 0}}
]

errorMessagePane[errorCode_] := With[{errorMessage = TextForm[tr["WolframCloudLoginErrors", errorCode]]}, 
	errorMessagePane[CurrentValue["WolframCloudUILogin"] && errorCode != 0, errorMessage]
]

  
End[]

EndPackage[]
