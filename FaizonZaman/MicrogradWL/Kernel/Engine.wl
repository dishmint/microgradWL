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

(* This is more work than needed, to mimic micrograd, I know, but it's useful for me to better understand and use more WL *)
(* 
    In[41]:= D[self + other, self]
    Out[41]= 1 
*)
PlusD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Self"]  := D[self + other, self]  /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}
PlusD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Other"] := D[self + other, other] /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}

(* 
    In[42]:= D[self other, self]
    Out[42]= other
*)
TimesD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Self"]  := D[self other, self]  /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}
TimesD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Other"] := D[self other, other] /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}

(* 
    In[43]:= D[self^other, self]
    Out[43]= other self^(-1 + other)
*)
PowerD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Self"]  := D[self^other, self]  /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}
PowerD[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue, "Other"] := D[self^other, other] /. { self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ], other -> PersistentSymbol[ b[ "Ref" ] ][ "Data" ]}

(* 
    In[19]:= D[Tanh[self], self]
    Out[19]= Sech[self]^2
 *)
TanhD[a_FaizonZaman`MicrogradWL`MGValue] := D[Tanh[self], self] /. self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ]

(* 
    In[24]:= D[Exp[self], self]
    Out[24]= E^self
 *)
ExpD[a_FaizonZaman`MicrogradWL`MGValue] := D[Exp[self], self] /. self -> PersistentSymbol[ a[ "Ref" ] ][ "Data" ]

(* ADD *)
MGValuePlus[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] + b["Data"], "Previous" -> { a["Ref"], b["Ref"] }, "Operator" -> "Plus"]
FaizonZaman`MicrogradWL`MGValue /: Plus[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValuePlus[ a, b ];
    ograd = PersistentSymbol[ out[ "Ref" ] ][ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += PlusD[ a, b, "Self"  ] * ograd;
                PersistentSymbol[ b[ "Ref" ] ][ "Grad" ] += PlusD[ a, b, "Other" ] * ograd;
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
                (* TimesD => D[self^other, _] *)
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += TimesD[ a, b, "Self"  ] * ograd;
                PersistentSymbol[ b[ "Ref" ] ][ "Grad" ] += TimesD[ a, b, "Other" ] * ograd;
                ],
            "_BackwardTimes"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]
FaizonZaman`MicrogradWL`MGValue /: Times[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] * b

(* POWER *)
MGValuePower[a_FaizonZaman`MicrogradWL`MGValue, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a["Data"] ^ b["Data"], "Previous" -> { a["Ref"], b["Ref"] }, "Operator" -> "Power"]
FaizonZaman`MicrogradWL`MGValue /: Power[a:FaizonZaman`MicrogradWL`MGValue[_], b:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValuePower[ a, b ];
    ograd = out[ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                (* PowerD => D[self^other, _] *)
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += (PowerD[ a, b, "Self" ] * ograd);
                PersistentSymbol[ b[ "Ref" ] ][ "Grad" ] += (PowerD[ a, b, "Other"] * ograd);
                ],
            "_BackwardPower"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]
FaizonZaman`MicrogradWL`MGValue /: Power[a_?NumberQ, b_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[a] ^ b

(* TANH *)
MGValueTanh[a_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[ Tanh[ a[ "Data" ] ], "Previous" -> { a["Ref"] }, "Operator" -> "Tanh"]
FaizonZaman`MicrogradWL`MGValue /: Tanh[a:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValueTanh[ a ];
    ograd = out[ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                (* TanhD => D[Tanh[self], self] *)
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += (TanhD[ a ] * ograd);
                ],
            "_BackwardTanh"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]

(* Exp *)
MGValueExp[a_FaizonZaman`MicrogradWL`MGValue] := FaizonZaman`MicrogradWL`MGValue[ Exp[ a[ "Data" ] ], "Previous" -> { a["Ref"] }, "Operator" -> "Exp"]
FaizonZaman`MicrogradWL`MGValue /: Exp[a:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {out, ograd},
    out = MGValueExp[ a ];
    ograd = out[ "Grad" ];
    PersistentSymbol[ out[ "Ref" ] ][ "_Backward" ] = Iconize[
            Function[
                (* ExpD => D[Exp[self], self] *)
                PersistentSymbol[ a[ "Ref" ] ][ "Grad" ] += (ExpD[ a ] * ograd);
                ],
            "_BackwardExp"
            ];
    out = FaizonZaman`MicrogradWL`MGValue[ PersistentSymbol[ out[ "Ref" ] ] ]
    ]

(* BACKWARD_MAIN *)
FaizonZaman`MicrogradWL`MGValue /: Backward[a:FaizonZaman`MicrogradWL`MGValue[_]] := Module[
    {topo},
    (* 1 - Build Topo *)
    topo = NestGraph[(FaizonZaman`MicrogradWL`MGValue /@ #["_Prev"])&, a];

    (* 2 - Call _Backward on each node in topo *)
    ]

End[]
EndPackage[]