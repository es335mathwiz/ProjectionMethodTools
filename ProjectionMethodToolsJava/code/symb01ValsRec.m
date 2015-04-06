BeginPackage["symb01ValsRec`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb01ValsRec package"]

(*compute z0 for time zero constraint only *)

Print["for one"]

aPath01ValsRec=genPath[1]

try01ValsRec={
(aPath01ValsRec[[5,1]]>=2/100&&Global`zzz$0$1[Global`t]==0)||
(aPath01ValsRec[[5,1]]==2/100&&Global`zzz$0$1[Global`t]>=0)
}
Export["try01ValsRecA.pdf",try01ValsRec[[1,1]]]
Export["try01ValsRecB.pdf",try01ValsRec[[1,2]]]

{symb01ValsRecFirstSecs,ig01ValsRec}=Timing[slv01ValsRec=Solve[try01ValsRec,{Global`zzz$0$1[Global`t]},Reals]//FullSimplify//Chop]
{symb01ValsRecSecondSecs,ig01ValsRec}=Timing[slv01ValsRec=Solve[try01ValsRec,{Global`zzz$0$1[Global`t]},Reals]//FullSimplify//Chop]
Global`zzz$0$1Func= Function @@ {{Global`qtm1,Global`rutm1,Global`eps},Piecewise[List @@@ (Last/@Flatten[slv01ValsRec])]}




makeInterpFunc[theFunc_Function]:=
FunctionInterpolation @@ {theFunc[Global`qq,Global`ru,Global`ep],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,
{Global`ep,-2*Global`sigma$u,2*Global`sigma$u}//.Global`lucaSubs//N,
InterpolationOrder -> 10, InterpolationPrecision -> 30, 
 AccuracyGoal -> 15, PrecisionGoal -> 15, 
 InterpolationPoints -> 100, MaxRecursion -> 25}



makeInterpFunc[theFunc_Function,iOrder_Integer,iPts_Integer]:=
Timing[FunctionInterpolation @@ {theFunc[Global`qq,Global`ru,Global`ep],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,
{Global`ep,-2*Global`sigma$u,2*Global`sigma$u}//.Global`lucaSubs//N,
InterpolationOrder ->iOrder, 
InterpolationPoints -> iPts(*, InterpolationPrecision -> 30, 
 AccuracyGoal -> 15, PrecisionGoal -> 15, MaxRecursion -> 25*)}]

Global`zzz$0$1InterpFunc=makeInterepFunc[Global`zzz$0$1Func]
(*
Splice["symb01ValsRecSecs.mtex"]
*)

Plot3D @@ 
{Global`zzz$0$1InterpFunc[Global`qq,Global`ru,0]-Global`zzz$0$1Func[Global`qq,Global`ru,0],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,PlotRange->All}

(*
Abs[Global`zzz$0$1InterpFunc[Global`qq,Global`ru,Global`ep]-Global`zzz$0$1Func[Global`qq,Global`ru,Global`ep]]
*)


infNorm[func_]:=
NMaximize @@ 
{{func[Global`qq,Global`ru,Global`ep],
(((Global`qLow<=Global`qq<=Global`qHigh)//.Global`lucaSubs)//N)&&
(((Global`ruLow<=Global`ru<=Global`ruHigh)//.Global`lucaSubs)//N)&&
(((-2*Global`sigma$u<=Global`ep<=2*Global`sigma$u)//.Global`lucaSubs)//N)
},
{Global`qq,Global`ru,Global`ep},Method->{"RandomSearch","SearchPoints"->50}}


experOrd[]:=
Module[{timeInterp=Flatten[
Table[Join[makeInterpFunc[Global`zzz$0$1Func,io,ip],{io,ip}],
{io,1,4},{ip,10,100,10}],1]},
{#[[1]],infNorm[cmpExct[#[[2]]]],#[[3]],#[[4]],#[[2]]}&/@ timeInterp
]

cmpExct[aFunc_]:=
Function[{Global`qq,Global`ru,Global`ep},Abs[aFunc[Global`qq,Global`ru,Global`ep]-Global`zzz$0$1Func[Global`qq,Global`ru,Global`ep]]]
Sort[experOrd[],#1[[2,1]]>#2[[2,1]]&]
(*

(*{io,1,5},{ip,5,50,5}*)

not really near zero norm  for (1,30)
Plot3D @@ 
{interOneVals[[3,-1]][Global`qq,Global`ru,0],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,PlotRange->All}



*)
(*
interOneVals=experOrd[]
Splice["interpOneCalcs.mtex"]
*)


EndPackage[]
Print["done reading symb01ValsRec package"]
