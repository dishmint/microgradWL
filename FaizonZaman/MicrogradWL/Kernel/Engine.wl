BeginPackage["FaizonZaman`MicrogradWL`Engine`"]

Begin["`Private`"]
(* 
    Needs["FaizonZaman`MicrogradWL``" -> "MGWL`"] 
    *)
(* Summarybox formatting for MGValue objects*)

FaizonZaman`MicrogradWL`MGValue /: MakeBoxes[obj: FaizonZaman`MicrogradWL`MGValue[asc_?MGValueAscQ], form: (StandardForm | TraditionalForm)] := Module[
    {above, below},
    (* Always show the data and grad *)
    above = {
        {If[asc["Label"] === "", Nothing, BoxForm`SummaryItem[{"Label: ", asc["Label"]}]], BoxForm`SummaryItem[{"Data: ", asc["Data"]}]},
        {BoxForm`SummaryItem[{"Grad: ", asc["Grad"]}], SpanFromLeft}
        };
    (* Hide the rest *)
    below = {
        BoxForm`SummaryItem[{"_Backward: ", asc["_Backward"]}],
        BoxForm`SummaryItem[{"_Prev: ", asc["_Prev"]}],
        BoxForm`SummaryItem[{"_Op: ", asc["_Op"]}]
    };
    BoxForm`ArrangeSummaryBox[
        FaizonZaman`MicrogradWL`MGValue,
        obj,
        $icon,
        above, below, form,
        "Interpretable" -> Automatic
        ]
    ];

MGValueAscQ[asc_?AssociationQ] := AllTrue[{"Data", "Grad", "_Backward", "_Prev", "_Op", "Label"}, KeyExistsQ[asc, #]&]
MGValueAscQ[_] = False
$icon = Graphics[
    {White, Text["MGV"]},
    ImageSize -> Dynamic[{Automatic, 3.5 CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}],
    Background -> Black
    ]
Options[FaizonZaman`MicrogradWL`MGValue] = {
    "Previous" -> Automatic,
    "Operator" -> Automatic,
    "Label" -> Automatic
};

FaizonZaman`MicrogradWL`MGValue[data_?NumberQ, opts:OptionsPattern[FaizonZaman`MicrogradWL`MGValue]] := Module[
    {
        prev = Replace[OptionValue["Previous"], Automatic -> {}],
        op = Replace[OptionValue["Operator"], Automatic -> ""],
        label = Replace[OptionValue["Label"], Automatic -> ""]
        },
    FaizonZaman`MicrogradWL`MGValue[uid] ^= FaizonZaman`MicrogradWL`MGValue[<| "Data" -> data, "Grad" -> 0, "_Backward" -> Identity, "_Prev" -> prev, "_Op" -> op, "Label" -> label |>]
    ]

FaizonZaman`MicrogradWL`MGValueQ[FaizonZaman`MicrogradWL`MGValue[asc_?MGValueAscQ]] := True
FaizonZaman`MicrogradWL`MGValueQ[_] := False

(* Properties *)
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Data" ] := asc[ "Data" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Grad" ] := asc[ "Grad" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Backward" ] := Switch[ asc[ "_Backward" ],
    _Function, asc[ "_Backward" ][],
    i_IconizedObject, FirstCase[ asc[ "_Backward" ], _Function ][]
    ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Prev" ] := asc[ "_Prev" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Op" ] := asc[ "_Op" ]

(* Setters *)
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Backward", f:(_Function | _IconizedObject) ] := FaizonZaman`MicrogradWL`MGValue[ReplacePart[ asc, Key[ "_Backward" ] -> f ]]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Grad", grad_?NumberQ ] := FaizonZaman`MicrogradWL`MGValue[ReplacePart[ asc, Key[ "Grad" ] -> grad ]]

(* TODO: Use `D` directly to compute the derivative *)
(* TODO: Would it be possible to use a symbol for Backward instead, like out = BackwardPlus ? I think the code will look cleaner that way *)
(* ADD *)
MGValuePlus[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] + b["Data"], "Previous" -> { a, b }, "Operator" -> "Plus"]
FaizonZaman`MicrogradWL`MGValue /: Plus[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValuePlus[ a, b ];
    ograd = out[ "Grad" ];
    out = out[
        "_Backward" ,
        Iconize[
            Function[
                a[ "Grad", a[ "Grad" ] + ograd ];
                b[ "Grad", b[ "Grad" ] + ograd ];
                ],
            "_BackwardPlus"
            ]
        ];
    out
    ]
FaizonZaman`MicrogradWL`MGValue /: Plus[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] + b

(* MULT *)
MGValueTimes[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] * b["Data"], "Previous" -> { a, b }, "Operator" -> "Times"]
FaizonZaman`MicrogradWL`MGValue /: Times[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValueTimes[ a, b ];
    ograd = out[ "Grad" ];
    out = out[
        "_Backward" ,
        Iconize[
            Function[
                a[ "Grad", b[ "Grad" ] * ograd ];
                b[ "Grad", a[ "Grad" ] * ograd ];
                ],
            "_BackwardTimes"
            ]
        ];
    out
    ]
FaizonZaman`MicrogradWL`MGValue /: Times[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] * b

End[]
EndPackage[]