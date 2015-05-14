BeginPackage["symb02ValsRec`",{"labDocPrep`","ProjectionInterface`","symb01ValsRec`"}]
Print["reading symb02ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]


aPath02ValsRec=genPath[02];


qrTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1InterpFunc[Global`qqTry,Global`ruTry,0]
}
qrPreTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1PreInterpFunc[Global`qqTry,Global`ruTry,0]
}
qrAccTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}




vars02=Cases[Variables[Level[qrTry02ValsRec[0,0],{-2}]],_[Global`t]]

qrTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrTry02ValsRec[Global`qqTry,Global`ruTry]}

qrTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

valRec02[Global`theFunc_,
Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
FixedPoint[Global`theFunc[#1[[1]],#1[[2]]][Global`qtm1,Global`rutm1,Global`eps]&,
{Global`qqTry,Global`ruTry}]

qrPreTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrPreTry02ValsRec[Global`qqTry,Global`ruTry]}

qrPreTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrPreTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

qrAccTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry02ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrAccTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]


















Print["construct more accurate interpolation"]
theOrd=4;thePts=100;
{interpTime02ValsRec,ig}=Timing[Global`zzz$0$1AccInterpFunc=Global`makeInterpFunc[Global`zzz$0$1PreInterpFunc,theOrd,thePts]];
igVar=Unique[];
{symb02ValsRecFirstSecs,igVar}=Timing[
valRec02[qrAccTry02ValsRecVals,.0341293,.015][-.1,-.08,.01]];
{symb02ValsRecSecondSecs,igVar}=Timing[
valRec02[qrAccTry02ValsRecVals,.0341293,.015][-.1,-.08,.01]];
Splice["symb02ValsRecSecs.mtex"]


EndPackage[]
Print["done reading symb02ValsRec package"]







