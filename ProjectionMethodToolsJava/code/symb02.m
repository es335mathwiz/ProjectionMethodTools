BeginPackage["symb02`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb02 package"]
(*compute z0 for time zero constraint only *)
Print["for two"]

aPath02=genPath[2]
try02={
((aPath02[[5,1]]>=(2/100)&&Global`zzz$1$1[Global`t]==0)||
(aPath02[[5,1]]==(2/100)&&Global`zzz$1$1[Global`t]>=0))&&
((aPath02[[8,1]]>=(2/100)&&Global`zzz$0$1[Global`t]==0)||
(aPath02[[8,1]]==(2/100)&&Global`zzz$0$1[Global`t]>=0))
}
Export["prettyEqns02A.pdf",
MatrixForm[(try02[[1,1]]//FullSimplify)//. Global`latexSubs]]
Export["prettyEqns02B.pdf",
MatrixForm[(try02[[1,2]]//FullSimplify)//. Global`latexSubs]]


try02Func=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02}
Print["first solve 02"]
{symb02FirstSecs,ig02}=Timing[soln02=Flatten[Solve[try02Func[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
Print["second  solve 02"]
{symb02SecondSecs,ig02}=Timing[soln02=Flatten[Solve[try02Func[Global`qtm1,Global`rutm1,Global`eps],{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]},Reals]]];
condExps=DeleteCases[Global`zzz$1$1[Global`t]/.#&/@soln02,Global`zzz$1$1[Global`t]]
Print["done solve 02"]
pw=Piecewise[((List @@ #)//Expand//Simplify)&/@condExps]//FullSimplify
try02ValsSoln= Function @@ {{Global`qtm1,Global`rutm1,Global`eps},pw}
Splice["symb02Secs.mtex"]




regs02=Union[Last/@(Last/@Flatten[soln02])]

regPlts02=RegionPlot3D[#,{Global`qtm1,-.5,.5},{Global`rutm1,-.2,.2},{Global`eps,-.1,.1}]& /@regs02


Global`redExport["prettyreg02pltA.pdf",regPlts02[[1]]]
Global`redExport["prettyreg02pltB.pdf",regPlts02[[2]]]
Global`redExport["prettyreg02pltC.pdf",regPlts02[[3]]]
Global`redExport["prettyreg02pltD.pdf",regPlts02[[4]]]



(*
Get["symb02Other.mth"]
*)


EndPackage[]
Print["done reading symb02 package"]
			
