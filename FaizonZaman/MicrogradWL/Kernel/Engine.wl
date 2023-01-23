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
        BoxForm`SummaryItem[{"ID: ", asc["ID"]}],
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

MGValueAscQ[asc_?AssociationQ] := AllTrue[{"ID", "Data", "Grad", "_Backward", "_Prev", "_Op", "Label"}, KeyExistsQ[asc, #]&]
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
        uid = Unique["$MGV"],
        prev = Replace[OptionValue["Previous"], Automatic -> {}],
        op = Replace[OptionValue["Operator"], Automatic -> ""],
        label = Replace[OptionValue["Label"], Automatic -> ""]
        },
    FaizonZaman`MicrogradWL`MGValue[uid] ^= FaizonZaman`MicrogradWL`MGValue[<| "ID" -> ToString[uid], "Data" -> data, "Grad" -> 0, "_Backward" -> Identity, "_Prev" -> prev, "_Op" -> op, "Label" -> label |>]
    ]

FaizonZaman`MicrogradWL`MGValueQ[FaizonZaman`MicrogradWL`MGValue[asc_?MGValueAscQ]] := True
FaizonZaman`MicrogradWL`MGValueQ[_] := False

(* Properties *)
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "ID" ] := asc[ "ID" ]
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

(* ADD *)
FaizonZaman`MicrogradWL`MGValue /: Plus[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = FaizonZaman`MicrogradWL`MGValue[a["Data"] + b["Data"], "Previous" -> {FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ], FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ]}, "Operator" -> "Plus"];
    ograd = out[ "Grad" ];
    out = out[
        "_Backward" ,
        Iconize[
            Function[
                FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ][ "Grad", FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ][ "Grad" ] + ograd ];
                FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ][ "Grad", FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ][ "Grad" ] + ograd ];
                ],
            "_Backward"
            ]
        ];
    out
    ]
FaizonZaman`MicrogradWL`MGValue /: Plus[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] + b

(* MULT *)
FaizonZaman`MicrogradWL`MGValue /: Times[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = FaizonZaman`MicrogradWL`MGValue[ a["Data"] * b["Data"],
        "Previous" -> {FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ], FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ]},
        "Operator" -> "Times"
        ];
    ograd = out[ "Grad" ];
    out = out[
        "_Backward" ,
        Iconize[
            Function[
                FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ][ "Grad", FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ][ "Grad" ] * ograd ];
                FaizonZaman`MicrogradWL`MGValue[ Symbol[ b[ "ID" ] ] ][ "Grad", FaizonZaman`MicrogradWL`MGValue[ Symbol[ a[ "ID" ] ] ][ "Grad" ] * ograd ];
                ],
            "_Backward"
            ]
        ];
    out
    ]
FaizonZaman`MicrogradWL`MGValue /: Times[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] * b

End[]
EndPackage[]