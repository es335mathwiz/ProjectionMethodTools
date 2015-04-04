BeginPackage["symb02Rec`",{"labDocPrep`","ProjectionInterface`","symb01`"}]
Print["reading symb02Rec package"]
(*compute z0 for time zero constraint only *)
Print["for two recursive"]

aPath02Rec=genPath[2]



try02Rec={
((aPath02Rec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02Rec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`zzz$0$1[Global`t]==Global`zzz$0$1Func[aPath02Rec[[4,1]],aPath02Rec[[6,1]],0]
}

Export["prettyEqns02A.pdf",
MatrixForm[(try02Rec[[1,1]]//FullSimplify)//. Global`latexSubs]]
Export["prettyEqns02B.pdf",
MatrixForm[(try02Rec[[1,2]]//FullSimplify)//. Global`latexSubs]]




try02RecFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02Rec}
try02RecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
{Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t]}/.Flatten[Solve[try02RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]
igVar=Unique[];
{symb02RecFirstSecs,igVar}=Timing[soln02Rec=Flatten[Solve[try02RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
{symb02RecSecondSecs,igVar}=Timing[soln02Rec=Flatten[Solve[try02RecFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
Splice["symb02RecSecs.mtex"]



Global`zzz$1$1Func[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{zSubs=Global`ridUndef[soln02Rec/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}]},
Global`zzz$1$1[Global`t]/.zSubs]

Global`zzz$1$1Func=Function  @@ 
{{Global`qtm1,Global`rutm1,Global`eps},
Piecewise[List @@@ Last /@  soln02Rec[[{1,3,5,7}]]]}


aPath02RecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[2,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try02RecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{Global`zzz$0$1[Global`t]:>tVals[[1]],
Global`zzz$1$1[Global`t]:>tVals[[2]]}]
hmatApp02Rec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath02RecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]]]//Chop]

(*
redExport["prettyrr02Rec.pdf",r02RecG=
Plot3D @@ {aPath02RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]],{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,
PlotLabel->
{"\!\(\*SubscriptBox[\(r\), \(t\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs//N]

redExport["prettyhapp02RecA.pdf",happ02RecAG=
Plot3D @@ {hmatApp02Rec[Global`qtm1,Global`rutm1,0][[2,1]]*(aPath02RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]]-.02),{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,PlotLabel->
{"\!\(\*SubscriptBox[\(H\), \(2\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs]

redExport["prettyhapp02RecB.pdf",happ02RecBG=
Plot3D @@ {hmatApp02Rec[Global`qtm1,Global`rutm1,0][[5,1]]*(aPath02RecExtFunc[Global`qtm1,Global`rutm1,0][[8,1]]-.02),{Global`qtm1,qLow,qHigh},{Global`rutm1,ruLow,ruHigh},
PlotRange->All,PlotLabel->
{"\!\(\*SubscriptBox[\(H\), \(5\)]\)[q,r,0]  constraint for two periods"}}//.lucaSubs]



FindMaximum @@ {
{Abs[hmatApp02Rec[Global`qtm1,Global`rutm1,0][[2,1]]*(aPath02RecExtFunc[Global`qtm1,Global`rutm1,0][[5,1]]-.02)],
And[qLow<= Global`qtm1<=qHigh,ruLow<=Global`rutm1<=ruHigh]}//.lucaSubs//N,
{{Global`qtm1,qLow},{Global`rutm1,ruLow}}//.lucaSubs//N}

FindMaximum @@ {
{Abs[hmatApp02Rec[Global`qtm1,Global`rutm1,0][[5,1]]*(aPath02RecExtFunc[Global`qtm1,Global`rutm1,0][[8,1]]-.02)],
And[qLow<= Global`qtm1<=qHigh,ruLow<=Global`rutm1<=ruHigh]}//.lucaSubs//N,
{{Global`qtm1,qLow},{Global`rutm1,ruLow}}//.lucaSubs//N}



*)

EndPackage[]
Print["done reading symb02Rec package"]
