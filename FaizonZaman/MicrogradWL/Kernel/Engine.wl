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
        BoxForm`SummaryItem[{"Ref: ", asc["Ref"]}],
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

MGValueAscQ[asc_?AssociationQ] := AllTrue[{"Ref", "Data", "Grad", "_Backward", "_Prev", "_Op", "Label"}, KeyExistsQ[asc, #]&]
MGValueAscQ[_] = False
$icon = Graphics[
    {White, Text["MGV"]},
    ImageSize -> Dynamic[{Automatic, 3.5 CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}],
    Background -> Black
    ]
Options[FaizonZaman`MicrogradWL`MGValue] = {
    "Grad" -> Automatic,
    "Previous" -> Automatic,
    "Operator" -> Automatic,
    "Label" -> Automatic
};

(* Instance Getter *)
FaizonZaman`MicrogradWL`MGValue /: (PersistentSymbol[MGValue[asc_?MGValueAscQ]][key_] = expr_) := MGValue[ PersistentSymbol[ asc[ "Ref" ] ][ key ] = expr ]
(* Constructor *)
FaizonZaman`MicrogradWL`MGValue[data_?NumberQ, opts:OptionsPattern[FaizonZaman`MicrogradWL`MGValue]] := Module[
    {
        grad = Replace[OptionValue["Grad"], Automatic -> 0],
        prev = Replace[OptionValue["Previous"], Automatic -> {}],
        op = Replace[OptionValue["Operator"], Automatic -> ""],
        label = Replace[OptionValue["Label"], Automatic -> ""]
        },
        With[
            {uid = ToString@Unique["$MGV"]},
            PersistentSymbol[uid] = <| "Ref" -> uid, "Data" -> data, "Grad" -> grad, "_Backward" -> Identity, "_Prev" -> prev, "_Op" -> op, "Label" -> label |>;
            FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[uid] ]
            ]
    ]

FaizonZaman`MicrogradWL`MGValueQ[FaizonZaman`MicrogradWL`MGValue[asc_?MGValueAscQ]] := True
FaizonZaman`MicrogradWL`MGValueQ[_] := False

(* Properties *)
FaizonZaman`MicrogradWL`MGValue[ref_String] := FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ ref ] ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Ref" ] := asc[ "Ref" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Data" ] := PersistentSymbol[ asc[ "Ref" ] ][ "Data" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "Grad" ] := PersistentSymbol[ asc[ "Ref" ] ][ "Grad" ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Backward" ] := Switch[ PersistentSymbol[ asc[ "Ref" ] ][ "_Backward" ],
    _Function, asc[ "_Backward" ][],
    i_IconizedObject, FirstCase[ asc[ "_Backward" ], _Function ][]
    ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Prev" ] := Map[ PersistentSymbol ][ asc[ "_Prev" ] ]
FaizonZaman`MicrogradWL`MGValue[ asc_?MGValueAscQ ][ "_Op" ] := asc[ "_Op" ]

(* TODO: Use `D` directly to compute the derivative *)
(* TODO: Would it be possible to use a symbol for Backward instead, like out = BackwardPlus ? I think the code will look cleaner that way *)
(* TODO: Might need to use _Prev in the _Backward to ensure those objects are affected by the function. *)
(* ADD *)
MGValuePlus[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] + b["Data"], "Previous" -> { a["Ref"], b["Ref"] }, "Operator" -> "Plus"]
FaizonZaman`MicrogradWL`MGValue /: Plus[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValuePlus[ a, b ];
    ograd = PersistentSymbol[ out[ "Ref" ] ][ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += ograd;
                PersistentSymbol[ b[ "Ref" ] ][ "Grad" ] += ograd;
                ],
            "_BackwardPlus"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]
FaizonZaman`MicrogradWL`MGValue /: Plus[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] + b

(* MULT *)
MGValueTimes[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] * b["Data"], "Previous" -> { a["Ref"], b["Ref"] }, "Operator" -> "Times"]
FaizonZaman`MicrogradWL`MGValue /: Times[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValueTimes[ a, b ];
    ograd = PersistentSymbol[ out[ "Ref" ] ][ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += PersistentSymbol[ b[ "Ref" ] ][ "Data" ] * ograd;
                PersistentSymbol[ b[ "Ref" ] ][ "Grad" ] += PersistentSymbol[ a[ "Ref" ] ][ "Data" ] * ograd;
                ],
            "_BackwardPlus"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]
FaizonZaman`MicrogradWL`MGValue /: Times[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] * b

(* TODO: POWER *)
(* MGValuePower[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] ^ b["Data"], "Previous" -> { a, b }, "Operator" -> "Power"]
FaizonZaman`MicrogradWL`MGValue /: Power[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValuePower[ a, b ];
    ograd = out[ "Grad" ];
    out = out[
        "_Backward" ,
        Iconize[
            Function[
                a[ "Grad", a[ "Grad" ] + (b[ "Data" ] * ( a[ "Data" ] ^ (b[ "Data" ] - 1)) * ograd) ];
                ],
            "_BackwardPower"
            ]
        ];
    out
    ]
FaizonZaman`MicrogradWL`MGValue /: Power[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] ^ b *)

End[]
EndPackage[]