(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["FaizonZaman`MicrogradWL`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


SayHello;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


SayHello[name_? StringQ] := "Hello " <> name <> "!";


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];