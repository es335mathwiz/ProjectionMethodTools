BeginPackage["symb04ValsRec`",{"labDocPrep`","ProjectionInterface`","symb03ValsRec`"}]
Print["reading symb04ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 04"]

aPath04ValsRec=genPath[04];

qrAccTry04ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath04ValsRec[[5,1]]>=0.02&&Global`zzz$3$1[Global`t]==0)||
(aPath04ValsRec[[5,1]]==0.02&&Global`zzz$3$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath04ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath04ValsRec[[6,1]]&&
Global`zzz$2$1[Global`t]==
Global`zzz$2$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$1$1[Global`t]==
Global`zzz$1$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}

vars04=Cases[Variables[Level[qrAccTry04ValsRec[0,0],{-2}]],_[Global`t]]

theOrd=1;thePts=4;
{interpTime04ValsRec,ig}=
Timing[{Global`zzz$0$1AccInterpFunc,Global`zzz$1$1AccInterpFunc,Global`zzz$2$1AccInterpFunc}=Global`makeInterpFunc[
Global`zzz$2$1PreInterpFunc,{-3,-2,-1},
theOrd,thePts,
{Global`qlv,Global`qhv},
{Global`qlv,Global`qhv},
{Global`elv,Global`ehv}]];
Print["done interpolation for zzz$2$1"];
igVar=Unique[];

qrAccTry04ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry04ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry04ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars04/.Flatten[NSolve[Identity[qrAccTry04ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars04,Reals]]

Print["construct more accurate interpolation"]
Global`zzz$3$1PreInterpFunc=With[{proc=Global`valRecN[qrAccTry04ValsRecVals]},
Function[{Global`qq,Global`ru,Global`eps},proc[Global`qq,Global`ru,Global`eps][[-1]]]]

Print["prepare for splicing 04"]
{symb04ValsRecFirstSecs,igVar}=Timing[
Global`valRecN[qrAccTry04ValsRecVals][-.1,-.08,.01]];
{symb04ValsRecSecondSecs,igVar}=Timing[
Global`valRecN[qrAccTry04ValsRecVals][-.1,-.08,.01]];
Splice["symb04ValsRecSecs.mtex"]


aPath04ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[04,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},qrVals=valRecN[qrAccTry04ValsRecVals][qtm1Arg,rutm1Arg,epsArg],
tVals=try04ValRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{
Global`zzz$0$1[Global`t]:>
		Global`zzz$0$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$1$1[Global`t]:>
		Global`zzz$1$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$2$1[Global`t]:>
		Global`zzz$2$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$3$1[Global`t]:>
		Global`zzz$3$1PreInterpFunc[qtm1Arg,rutm1Arg,0]
}]

hmatApp04ValRec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath04ValsRecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]],Global`hmat.tp[[Range[9]+6]]]//Chop]


EndPackage[]
Print["done reading symb04ValsRec package"]







