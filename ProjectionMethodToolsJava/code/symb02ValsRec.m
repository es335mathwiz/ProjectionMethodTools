BeginPackage["symb02ValsRec`",{"labDocPrep`","ProjectionInterface`","symb01ValsRec`"}]
Print["reading symb02ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]


aPath02ValsRec=genPath[02];
qrAccTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}





qrAccTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry02ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve @@ {qrAccTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps],vars02,Reals}]

vars02=Cases[Variables[Level[qrAccTry02ValsRec[0,0],{-2}]],_[Global`t]]




Print["Global`zzz$0$1PreInterpFunc not a list yet like others will be"]
igVar=Unique[];

{interpTime02ValsRec,ig}=
Timing[Global`zzz$0$1AccInterpFunc=Global`makeInterpFunc[
Global`zzz$0$1PreInterpFunc,
Global`theOrd,Global`thePts,
{Global`qlv,Global`qhv},
{Global`qlv,Global`qhv},
{Global`elv,Global`ehv}]];
Print["done interpolation for zzz$0$1"]

Print["construct more accurate interpolation"]
Global`zzz$1$1PreInterpFunc=With[{proc=Global`valRecN[qrAccTry02ValsRecVals]},
Function[{Global`qq,Global`ru,Global`eps},proc[Global`qq,Global`ru,Global`eps]]]

Print["prepare for splicing 02"]
{symb02ValsRecFirstSecs,igVar}=Timing[
Global`valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
{symb02ValsRecSecondSecs,igVar}=Timing[
Global`valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
Splice["symb02ValsRecSecs.mtex"]



compZs[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{qrVals=qrAccTry02ValsRecVals[qtm1Arg,rutm1Arg,epsArg]},
{Global`zzz$0$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
		Global`zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0]}]



aPath02ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[02,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},qrVals=qrAccTry02ValsRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{
Global`zzz$0$1[Global`t]:>
		Global`zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0][[-2]],
Global`zzz$1$1[Global`t]:>
		Global`zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0][[-1]]
}]

hmatApp02ValRec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath02ValsRecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]]]//Chop]





EndPackage[]
Print["done reading symb02ValsRec package"]










(*


ha=Function[{qq$, ru$, eps$}, 
      Function[{qtm1$, rutm1$, eps$}, 
        FixedPoint[qrAccTry02ValsRecVals[#1[[1]], #1[[2]]][qtm1$, rutm1$, 
            eps$] & , {0, 0}][[-1]]][qq$, ru$, eps$]]

  FunctionInterpolation[Sow[ha[interpqq,interpru,interpep]], {interpqq, qlv, qhv}, {interpru, rlv, rhv},{interpep, elv, ehv}, InterpolationOrder -> 1, InterpolationPoints -> 5] 


*)




(*

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


*)
(*
qrTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrTry02ValsRec[Global`qqTry,Global`ruTry]}

qrTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]





qrPreTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrPreTry02ValsRec[Global`qqTry,Global`ruTry]}

qrPreTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrPreTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]



*)
