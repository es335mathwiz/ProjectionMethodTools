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

Global`valRecN[Global`theFunc_]:=
Function[{Global`qtm1,Global`rutm1,Global`eps},
With[{initVals=Global`primeFunc[Global`qtm1,Global`rutm1,Global`eps],
fixFunc=With[{fixVal=Global`theFunc[#1[[1]],#1[[2]]][Global`qtm1,Global`rutm1,Global`eps]},Sow[fixVal,"fixVal"];
fixVal]&},Sow[initVals,"initVals="];Sow[{Global`qtm1,Global`rutm1,Global`eps},"for state="];
With[{theVal=FixedPoint[
fixFunc,{initVals[[1]],initVals[[2]]}]},Sow[theVal,"theVal"];theVal[[-1]]]]]





qrPreTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrPreTry02ValsRec[Global`qqTry,Global`ruTry]}

qrPreTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrPreTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

qrAccTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry02ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve @@ {qrAccTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps],vars02,Reals}]


















Print["construct more accurate interpolation"]
Global`zzz$1$1PreInterpFunc=With[{proc=Global`valRecN[qrAccTry02ValsRecVals]},
Function[{Global`qq,Global`ru,Global`eps},proc[Global`qq,Global`ru,Global`eps]]]
theOrd=4;thePts=100;
{interpTime02ValsRec,ig}=
Timing[Global`zzz$0$1AccInterpFunc=Global`makeInterpFunc[Global`zzz$0$1PreInterpFunc,theOrd,thePts,
{Global`qlv,Global`qhv},
{Global`qlv,Global`qhv},
{Global`elv,Global`ehv}]];
Print["done interpolation for zzz$0$1"]
igVar=Unique[];
Print["prepare for splicing 02"]
{symb02ValsRecFirstSecs,igVar}=Timing[
Global`valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
{symb02ValsRecSecondSecs,igVar}=Timing[
Global`valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
Splice["symb02ValsRecSecs.mtex"]


EndPackage[]
Print["done reading symb02ValsRec package"]










(*


ha=Function[{qq$, ru$, eps$}, 
      Function[{qtm1$, rutm1$, eps$}, 
        FixedPoint[qrAccTry02ValsRecVals[#1[[1]], #1[[2]]][qtm1$, rutm1$, 
            eps$] & , {0, 0}][[-1]]][qq$, ru$, eps$]]

  FunctionInterpolation[Sow[ha[interpqq,interpru,interpep]], {interpqq, qlv, qhv}, {interpru, rlv, rhv},{interpep, elv, ehv}, InterpolationOrder -> 1, InterpolationPoints -> 5] 


*)
