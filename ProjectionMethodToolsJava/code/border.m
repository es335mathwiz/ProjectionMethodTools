<<JavaGraphics`
Needs["symb02ValsRec`"]
forBorder=(try02ValsRec[[1,3,2]]&&Global`zzz$0$1[t]>=0)/.{Global`zzz$1$1[t]->0}//FullSimplify
(*
z0AtBorder=solnCondsToPiecewise[Solve[forBorder,Global`zzz$0$1[t]]//FullSimplify//Chop]

z0AtBorder=Flatten[Reduce[forBorder,Global`zzz$0$1[t],Reals]//FullSimplify//Chop]
*)
z0AtBorder=Flatten[solnCondsToPiecewise[Solve[forBorder,Global`zzz$0$1[t],Reals]]//FullSimplify//Chop]

theBorder=
((try02ValsRec[[1]]/.z0AtBorder//FullSimplify//Chop)/.Global`zzz$1$1[t]->0)//FullSimplify//Chop

epsBorder[qq_?NumberQ,ru_?NumberQ]:=eps/.Flatten[Solve[theBorder/.{qtm1->qq,rutm1->ru},eps]]


ParametricPlot3D[{qtm1,rutm1,epsBorder[qtm1,rutm1]},{qtm1,qlv,qhv},{rutm1,rlv,rhv}]
(*below border know  z1=0 *)





(*then above border solve z1, know at constraint*)
forZ1Pos=(try02ValsRec[[1,3,2]]&&Global`zzz$0$1[t]>=0&&Global`zzz$1$1[t]>=0)/.(try02ValsRec[[1,4,1]]->try02ValsRec[[1,4,2]])//FullSimplify


Reduce[forZ1Pos,{zzz$0$1[t],zzz$1$1[t]},Reals]//FullSimplify//Chop


huh=solnCondsToPiecewise[Solve[forZ1Pos,{zzz$0$1[t],zzz$1$1[t]},Reals]//FullSimplify//Chop]
