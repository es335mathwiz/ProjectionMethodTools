BeginPackage["symb03`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb03 package"]
(*compute z0 for time zero constraint only *)
Print["for three"]


aPath03=genPath[3];


try03={
((aPath03[[5,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath03[[5,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
((aPath03[[8,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath03[[8,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath03[[11,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath03[[11,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}
Export["prettyEqns03A.pdf",
MatrixForm[(try03[[1,1]]//FullSimplify)//. Global`latexSubs]]
Export["prettyEqns03B.pdf",
MatrixForm[(try03[[1,2]]//FullSimplify)//. Global`latexSubs]]

abStr="Aborted("<>ToString[Global`$MaxSolveTime]<>")";
try03Func=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try03}
Print["trying first solve"]
Print["time limit set to ",Global`$MaxSolveTime]
If[
TimeConstrained[{symb03FirstSecs,ig02}=Timing[soln03=Flatten[Solve[try03Func[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]],Global`$MaxSolveTime]===$Aborted,
symb03FirstSecs=abStr];
Print["trying second solve"]
If[symb03FirstSecs===abStr,symb03SecondSecs=abStr,
{symb03SecondSecs,ig02}=Timing[soln03=Flatten[Solve[try03Func[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]]];
Print["done trying second solve"]
condExps=DeleteCases[Global`zzz$1$1[Global`t]/.#&/@soln03,Global`zzz$1$1[Global`t]]
pw=Piecewise[((List @@ #)//Expand//Simplify)&/@condExps]//FullSimplify
try03ValsSoln= Function @@ {{Global`qtm1,Global`rutm1,Global`eps},pw}
Splice["symb03Secs.mtex"]
EndPackage[]
Print["done reading symb03 package"]
