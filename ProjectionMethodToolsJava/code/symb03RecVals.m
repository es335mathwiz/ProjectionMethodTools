BeginPackage["symb03Rec`",{"labDocPrep`","ProjectionInterface`","symb01`","symb02Rec`"}]
Print["reading symb03Rec package"]
(*compute z0 for time zero constraint only *)
Print["for three recursive"]

aPath03Rec=genPath[3]



try03Rec={
((aPath03Rec[[5,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath03Rec[[5,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
Global`zzz$1$1[Global`t]==
Global`zzz$1$1Func[aPath03Rec[[4,1]],aPath03Rec[[6,1]],0]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1Func[aPath03Rec[[7,1]],aPath03Rec[[9,1]],0]
}

Export["prettyEqns03A.pdf",
MatrixForm[(try03Rec[[1,1]]//FullSimplify)//. Global`latexSubs]]
Export["prettyEqns03B.pdf",
MatrixForm[(try03Rec[[1,2]]//FullSimplify)//. Global`latexSubs]]




try03RecFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try03Rec}
try03RecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{
{Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$2$1[Global`t]}/.
Flatten[Solve[
try03RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]
igVar=Unique[];

(*
{symb03RecFirstSecs,igVar}=Timing[soln03Rec=Flatten[Solve[try03RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
{symb03RecSecondSecs,igVar}=Timing[soln03Rec=Flatten[Solve[try03RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
Splice["symb03RecSecsNot.mtex"]





aPath03RecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[2,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try03RecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{Global`zzz$0$1[Global`t]:>tVals[[1]],
Global`zzz$1$1[Global`t]:>tVals[[2]]}]
hmatApp03Rec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath03RecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]]]//Chop]

(*
redExport["prettyrr03Rec.pdf",r03RecG=
Plot3D @@ {aPath03RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]],{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,
PlotLabel->
{"\!\(\*SubscriptBox[\(r\), \(t\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs//N]

redExport["prettyhapp03RecA.pdf",happ03RecAG=
Plot3D @@ {hmatApp03Rec[Global`qtm1,Global`rutm1,0][[2,1]]*(aPath03RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]]-.02),{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,PlotLabel->
{"\!\(\*SubscriptBox[\(H\), \(2\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs]

redExport["prettyhapp03RecB.pdf",happ03RecBG=
Plot3D @@ {hmatApp03Rec[Global`qtm1,Global`rutm1,0][[5,1]]*(aPath03RecExtFunc[Global`qtm1,Global`rutm1,0][[8,1]]-.02),{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,PlotLabel->
{"\!\(\*SubscriptBox[\(H\), \(5\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs]



FindMaximum @@ {
{Abs[hmatApp03Rec[Global`qtm1,Global`rutm1,0][[2,1]]*(aPath03RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]]-.02)],
And[qLow<= Global`qtm1<=qHigh,ruLow<=Global`rutm1<=ruHigh]}//.lucaSubs//N,
{{Global`qtm1,qLow},{Global`rutm1,ruLow}}//.lucaSubs//N}

FindMaximum @@ {
{Abs[hmatApp03Rec[Global`qtm1,Global`rutm1,0][[5,1]]*(aPath03RecExtFunc[Global`qtm1,Global`rutm1,0][[8,1]]-.02)],
And[qLow<= Global`qtm1<=qHigh,ruLow<=Global`rutm1<=ruHigh]}//.lucaSubs//N,
{{Global`qtm1,qLow},{Global`rutm1,ruLow}}//.lucaSubs//N}



*)

*)

EndPackage[]
Print["done reading symb03Rec package"]
