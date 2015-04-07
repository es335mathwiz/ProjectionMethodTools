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

Print["first solve"]
{symb01ValsRecFirstSecs,ig01ValsRec}=Timing[slv01ValsRec=Solve[try01ValsRec,{Global`zzz$0$1[Global`t]},Reals]//FullSimplify//Chop]
Print["second solve"]
{symb01ValsRecSecondSecs,ig01ValsRec}=Timing[slv01ValsRec=Solve[try01ValsRec,{Global`zzz$0$1[Global`t]},Reals]//FullSimplify//Chop]
Print["construct zzz$0$1Func"]
Global`zzz$0$1PreInterpFunc= Function @@ {{Global`qtm1,Global`rutm1,Global`eps},Piecewise[List @@@ (Last/@Flatten[slv01ValsRec])]}
Splice["symb01ValsRecSecs.mtex"]

Print["construct interpolation"]
Global`zzz$0$1InterpFunc=Global`makeInterpFunc[Global`zzz$0$1PreInterpFunc]



(*
Plot3D @@ 
{Global`zzz$0$1InterpFunc[Global`qq,Global`ru,0]-Global`zzz$0$1Func[Global`qq,Global`ru,0],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,PlotRange->All}

Abs[Global`zzz$0$1InterpFunc[Global`qq,Global`ru,Global`ep]-Global`zzz$0$1Func[Global`qq,Global`ru,Global`ep]]

(*{io,1,5},{ip,5,50,5}*)
not really near zero norm  for (1,30)
Plot3D @@ 
{interOneVals[[3,-1]][Global`qq,Global`ru,0],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,PlotRange->All}
*)
(*
Timing[interOneVals=experOrd[]]
Splice["interpOneCalcs.mtex"]
Sort[experOrd[],#1[[2,1]]>#2[[2,1]]&]
*)


EndPackage[]
Print["done reading symb01ValsRec package"]
