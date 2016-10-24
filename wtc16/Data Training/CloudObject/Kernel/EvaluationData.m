BeginPackage["CloudObject`"]

System`EvaluationData;

Begin["`Private`"]
(* EvaluationData *)

If[DownValues[EvaluationData] === {},
	SetAttributes[EvaluationData, HoldAllComplete];
	EvaluationData[expr_] :=
		Module[{walltime, cputime, $messages, handleMessage, messageList,
			success, messageParts, result, data},
			$messages = Internal`Bag[];
			handleMessage = logMessage[$messages, #]&;
			Internal`AddHandler["Message", handleMessage];

			AbsoluteTiming[Timing[Catch[Catch[expr, _]]]] /. {
				{wall_, {cpu_, res__}} :> 
				(* we use res__ because expr could evaluate to a Sequence, 
					see https://jira.wolfram.com/jira/browse/SAAS-11630 *)
				(
					walltime = wall;
					cputime = cpu;
					result = res
				)
			};

			messageList = Internal`BagPart[$messages, All];
			success = messageList === {};
			(* get a triple for Message, MessagesText, and MessagesExpressions*)
			messageParts = If[messageList === {},
				{{}, {}, {}},
				Transpose[messageList]
			];

			data = <|
				"Result" -> result,
				"Success" -> success,
				"FailureType" -> If[success, None, "MessageFailure"], (* this needs to be designed *)
				(*"InputString" -> input, *) (* temporarily disabled *)
				"Messages" -> messageParts[[1]],
				"MessagesText" -> messageParts[[2]],
				"MessagesExpressions" -> messageParts[[3]],
				"Timing" -> Round[cputime, 0.001],
				"AbsoluteTiming" -> Round[walltime, 0.001]
			|>;

			If[!TrueQ[CloudObject`Private`$DisableEvaluationDataToString],
				data = Append[data,
					"InputString" -> ToString[Unevaluated[expr], InputForm]]; (* this blows up when submitting forms on devel, no idea why yet *)
			];

			Internal`RemoveHandler["Message", handleMessage];

			data
		];

	Protect[EvaluationData];

	logMessage[bag_, Hold[Message[___], False]] := Null; (* skip *)

	(* always skip General::newsym, it is on in webMathematica and is way too noisy for us *)
	logMessage[bag_, Hold[Message[General::newsym, ___], _]] := Null;

	logMessage[bag_, expr:Hold[Message[tag_, ___], True]] :=
		With[{heldmsg = Delete[expr, -1]},
			Internal`StuffBag[bag, {tag, messageToString[heldmsg], heldmsg}]
		];

	messageToString[Hold[Message[msgname_, args___]]] :=
		(* if msgname is an unevaluated MessageName, it means the message
			template is defined as General::tag *)
		ToString[Row[{ToString[Unevaluated[msgname]], " : ",
			StringForm[If[Head[msgname] === MessageName,
				ReplacePart[msgname, 1 -> General], msgname], args]
	    }]];
];

End[]

EndPackage[]
