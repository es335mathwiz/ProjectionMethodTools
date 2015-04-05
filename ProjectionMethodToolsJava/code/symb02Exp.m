BeginPackage["symb02Exp`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb02Exp package"]
(*compute z0 for time zero constraint only *)
Print["for two"]

aPath02Exp=genValsPath[2]
try02Exp={
((aPath02Exp[[5,1]]>=(2/100)&&Global`zzz$1$1[Global`t]==0)||
(aPath02Exp[[5,1]]==(2/100)&&Global`zzz$1$1[Global`t]>=0))&&
((aPath02Exp[[8,1]]>=(2/100)&&Global`zzz$0$1[Global`t]==0)||
(aPath02Exp[[8,1]]==(2/100)&&Global`zzz$0$1[Global`t]>=0))
}
Export["prettyEqns02ExpA.pdf",
MatrixForm[(try02Exp[[1,1]]//FullSimplify)//. Global`latexSubs]]
Export["prettyEqns02ExpB.pdf",
MatrixForm[(try02Exp[[1,2]]//FullSimplify)//. Global`latexSubs]]


try02ExpFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02Exp}
Print["first solve 02Exp"]
{symb02ExpFirstSecs,ig02Exp}=Timing[soln02Exp=Flatten[Solve[try02ExpFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
Print["second  solve 02Exp"]
{symb02ExpSecondSecs,ig02Exp}=Timing[soln02Exp=Flatten[Solve[try02ExpFunc[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
condExps=DeleteCases[Global`zzz$1$1[Global`t]/.#&/@soln02Exp,Global`zzz$1$1[Global`t]]
Print["done solve 02Exp"]
pw=Piecewise[((List @@ #)//Expand//Simplify)&/@condExps]//FullSimplify
try02ExpValsSoln= Function @@ {{Global`qtm1,Global`rutm1,Global`eps},pw}

Expectation[try02ExpValsSoln[qtm1,rutm1,0]/.epsVar$0$1[t]->0,epsVar$1$1[t] \[Distributed] NormalDistribution[0,.1]]

Splice["symb02ExpSecs.mtex"]






regs02Exp=Union[Last/@(Last/@Flatten[soln02Exp])]

regPlts02Exp=RegionPlot3D[#,{Global`qtm1,-.5,.5},{Global`rutm1,-.2,.2},{Global`eps,-.1,.1}]& /@regs02Exp


Global`redExport["prettyreg02ExppltA.pdf",regPlts02Exp[[1]]]
Global`redExport["prettyreg02ExppltB.pdf",regPlts02Exp[[2]]]
Global`redExport["prettyreg02ExppltC.pdf",regPlts02Exp[[3]]]
Global`redExport["prettyreg02ExppltD.pdf",regPlts02Exp[[4]]]



(*
Get["symb02ExpOther.mth"]
*)


EndPackage[]
Print["done reading symb02Exp package"]
			
