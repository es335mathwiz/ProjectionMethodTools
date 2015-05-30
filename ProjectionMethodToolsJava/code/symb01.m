BeginPackage["symb01`",{"ProtectedSymbols`","occBindRecur`","ProjectionInterface`"}]
Print["reading symb01 package"]
(*compute z0 for time zero constraint only *)
Print["for one"]
Print["generate a path of minimal length to impose constraint one period"]
aPath01=Private`genPath[1]
try01={
(aPath01[[5,1]]>=0.02&&zzz$0$1[t]==0)||
(aPath01[[5,1]]==0.02&&zzz$0$1[t]>=0)
}
Export["try01A.pdf",try01[[1,1]]]
Export["try01B.pdf",try01[[1,2]]]

{symb01FirstSecs,ig01}=Timing[slv01=Solve[try01,{zzz$0$1[t]},Reals]//FullSimplify//Chop]
{symb01SecondSecs,ig01}=Timing[slv01=Solve[try01,{zzz$0$1[t]},Reals]//FullSimplify//Chop]
zzz$0$1Func= Function @@ {{qtm1,rutm1,eps},Piecewise[List @@@ (Last/@Flatten[slv01])]}

Splice["symb01Secs.mtex"]

Print["presentation computions for one period"]
red01=Reduce[try01,{zzz$0$1[t]},Reals]//FullSimplify//Chop
Export["red01A.pdf",red01[[1,1]]]
Export["red01B.pdf",red01[[1,2]]]
Export["red01C.pdf",red01[[2]]]

hmatApp01NotFunc=hmat .(aPath01Ext=Private`genPath[1,1]/.zzz$0$1[t]->zzz$0$1Func[qtm1,rutm1,eps]//FullSimplify//Chop)//FullSimplify//Chop
Export["prettyhmatApp01.pdf",MatrixForm[hmatApp01NotFunc//.latexSubs/.morePaperSubs]]
Export["prettyPath01.pdf",MatrixForm[aPath01Ext//.latexSubs/.morePaperSubs]]

Export["prettyEqns01.pdf",
MatrixForm[(Transpose[{eqns01=doRecurIneqOccEqns[{}]/.{eqvdIf->If,GreaterEqual->Greater}}]//Simplify)//.latexSubs]
]
(*change conditionalExpression of two cases to form to get to piecewise*)
pairs=Partition[Sort[Flatten[
Solve[Thread[eqns01==0],{qq[t],ru[t],rr[t],discrep[t],zzz$0$1[t]},Reals]]],2]

soln01=(Piecewise[List @@@ Last /@ #]//FullSimplify//Chop)&/@pairs

Export["prettySoln01Q.pdf", MatrixForm[soln01[[2]]//Simplify/.latexSubs]]
Export["prettySoln01R.pdf", MatrixForm[soln01[[3]]//Simplify/.latexSubs]]
Export["prettySoln01Z.pdf", MatrixForm[soln01[[5]]//Simplify/.latexSubs]]

regs01=Last/@(Last/@Flatten[slv01])

regPlts01=RegionPlot3D[#,{qtm1,-.5,.5},{rutm1,-.2,.2},{eps,-.1,.1}]& /@regs01


redExport["prettyreg01pltA.pdf",regPlts01[[1]]]
redExport["prettyreg01pltB.pdf",regPlts01[[2]]]

EndPackage[]
Print["done reading symb01 package"]
