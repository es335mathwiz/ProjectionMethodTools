BeginPackage["symb02ValsRec`",{"labDocPrep`","ProjectionInterface`","symb01ValsRec`"}]
Print["reading symb02ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]


aPath02ValsRec=Private`genPath[02];
qrAccTry02ValsRec[qqTry_?NumberQ,ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&zzz$1$1[t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&zzz$1$1[t]>=0))&&
qq[t]==aPath02ValsRec[[4,1]]&&
ru[t]==aPath02ValsRec[[6,1]]&&
zzz$0$1[t]==
zzz$0$1AccInterpFunc[qqTry,ruTry,0]
}





qrAccTry02ValsRecFunc[qqTry_?NumberQ,ruTry_?NumberQ]:=Function @@ {{qtm1,rutm1,eps},qrAccTry02ValsRec[qqTry,ruTry]}

qrAccTry02ValsRecVals[qqTry_?NumberQ,ruTry_?NumberQ][qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
vars02/.Flatten[NSolve @@ {qrAccTry02ValsRecFunc[qqTry,ruTry][qtm1,rutm1,eps],vars02,Reals}]

vars02=Cases[Variables[Level[qrAccTry02ValsRec[0,0],{-2}]],_[t]]




Print["zzz$0$1PreInterpFunc not a list yet like others will be"]
igVar=Unique[];

{interpTime02ValsRec,ig}=
Timing[zzz$0$1AccInterpFunc=makeInterpFunc[
zzz$0$1PreInterpFunc,
theOrd,thePts,
{qlv,qhv},
{qlv,qhv},
{elv,ehv}]];
Print["done interpolation for zzz$0$1"]

Print["construct more accurate interpolation"]
zzz$1$1PreInterpFunc=With[{proc=valRecN[qrAccTry02ValsRecVals]},
Function[{qq,ru,eps},proc[qq,ru,eps]]]

Print["prepare for splicing 02"]
{symb02ValsRecFirstSecs,igVar}=Timing[
valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
{symb02ValsRecSecondSecs,igVar}=Timing[
valRecN[qrAccTry02ValsRecVals][-.1,-.08,.01]];
Splice["symb02ValsRecSecs.mtex"]



compZs[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{qrVals=qrAccTry02ValsRecVals[qtm1Arg,rutm1Arg,epsArg]},
{zzz$0$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
		zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0]}]



aPath02ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=Private`genPath[02,1]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},qrVals=qrAccTry02ValsRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{
zzz$0$1[t]:>
		zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0][[-2]],
zzz$1$1[t]:>
		zzz$1$1PreInterpFunc[qrVals[[1]],qrVals[[2]],0][[-1]]
}]

hmatApp02ValRec[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
With[{tp=aPath02ValsRecExtFunc[qtm1,rutm1,eps]},
Join[hmat.tp[[Range[9]]],hmat.tp[[Range[9]+3]]]//Chop]





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

qrTry02ValsRec[qqTry_?NumberQ,ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&zzz$1$1[t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&zzz$1$1[t]>=0))&&
qq[t]==aPath02ValsRec[[4,1]]&&
ru[t]==aPath02ValsRec[[6,1]]&&
zzz$0$1[t]==
zzz$0$1InterpFunc[qqTry,ruTry,0]
}
qrPreTry02ValsRec[qqTry_?NumberQ,ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&zzz$1$1[t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&zzz$1$1[t]>=0))&&
qq[t]==aPath02ValsRec[[4,1]]&&
ru[t]==aPath02ValsRec[[6,1]]&&
zzz$0$1[t]==
zzz$0$1PreInterpFunc[qqTry,ruTry,0]
}


*)
(*
qrTry02ValsRecFunc[qqTry_?NumberQ,ruTry_?NumberQ]:=Function @@ {{qtm1,rutm1,eps},qrTry02ValsRec[qqTry,ruTry]}

qrTry02ValsRecVals[qqTry_?NumberQ,ruTry_?NumberQ][qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrTry02ValsRecFunc[qqTry,ruTry][qtm1,rutm1,eps]],vars02,Reals]]





qrPreTry02ValsRecFunc[qqTry_?NumberQ,ruTry_?NumberQ]:=Function @@ {{qtm1,rutm1,eps},qrPreTry02ValsRec[qqTry,ruTry]}

qrPreTry02ValsRecVals[qqTry_?NumberQ,ruTry_?NumberQ][qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrPreTry02ValsRecFunc[qqTry,ruTry][qtm1,rutm1,eps]],vars02,Reals]]



*)
