(* Mathematica Package *)

BeginPackage["ProtectedSymbols`"]
(* Exported symbols added here with SymbolName::usage *)  
t::usage="symbol for time in models"
eps::usage="symbol for stochastic variables in models"
Protect[t,eps]
makeProtectedSymbol::usage="makeProtectedSymbol[symbName_String]  no context paths allowed"
Begin["`Private`"] (* Begin Private Context *) 

makeProtectedSymbol[symbName_String]:=
With[{symb=ToExpression["ProtectedSymbols`"<>symbName]},Protect[symb];symb]/;StringFreeQ[symbName,"`"];
makeProtectedSymbol[___]:=Print[makeProtectedSymbol::usage]

End[] (* End Private Context *)

EndPackage[]