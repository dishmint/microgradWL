(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["FaizonZaman`MicrogradWL`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


MGValue::usage = "Represents a value in micrograd";
MGValueQ::usage = "Tests whether a value is a valid micrograd MGValue";
$MGValues::usage = "Returns the list of MGValues"

Begin["`Private`"];
(* LOAD IN ENGINE AND NN PACKAGES *)
Needs["FaizonZaman`MicrogradWL`Engine`"]
Needs["FaizonZaman`MicrogradWL`NN`"]

(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)

(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];