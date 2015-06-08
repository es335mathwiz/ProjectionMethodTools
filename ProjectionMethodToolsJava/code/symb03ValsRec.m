BeginPackage["symb03ValsRec`",{"labDocPrep`","ProjectionInterface`","symb02ValsRec`"}]
Print["reading symb03ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 03"]

aPath03ValsRec=genPath[03];

qrAccTry03ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath03ValsRec[[5,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath03ValsRec[[5,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath03ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath03ValsRec[[6,1]]&&
Global`zzz$1$1[Global`t]==
Global`zzz$1$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}

vars03=Cases[Variables[Level[qrAccTry03ValsRec[0,0],{-2}]],_[Global`t]]
igVar=Unique[];
{interpTime03ValsRec,igVar}=
Timing[{Global`zzz$0$1AccInterpFunc,Global`zzz$1$1AccInterpFunc}=Global`makeInterpFunc[
Global`zzz$1$1PreInterpFunc,{-2,-1},
Global`theOrd,Global`thePts,
{Global`qlv,Global`qhv},
{Global`qlv,Global`qhv},
{Global`elv,Global`ehv}]];
Print["done interpolation for zzz$1$1"];

qrAccTry03ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry03ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry03ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars03/.Flatten[NSolve[Identity[qrAccTry03ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars03,Reals]]

Print["construct more accurate interpolation"]
Global`zzz$2$1PreInterpFunc=With[{proc=Global`valRecN[qrAccTry03ValsRecVals]},
Function[{Global`qq,Global`ru,Global`eps},proc[Global`qq,Global`ru,Global`eps]]]

Print["prepare for splicing 03"]
{symb03ValsRecFirstSecs,igVar}=Timing[
Global`valRecN[qrAccTry03ValsRecVals][-.1,-.08,.01]];
{symb03ValsRecSecondSecs,igVar}=Timing[
Global`valRecN[qrAccTry03ValsRecVals][-.1,-.08,.01]];
Splice["symb03ValsRecSecs.mtex"]


aPath03ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[03,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},qrVals=Global`valRecN[qrAccTry03ValsRecVals][qtm1Arg,rutm1Arg,epsArg],
tVals=try03ValRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{
Global`zzz$0$1[Global`t]:>
		Global`zzz$2$1PreInterpFunc[qtm1Arg,rutm1Arg,0][[-3]],
Global`zzz$1$1[Global`t]:>
		Global`zzz$2$1PreInterpFunc[qtm1Arg,rutm1Arg,0][[-2]],
Global`zzz$2$1[Global`t]:>
		Global`zzz$2$1PreInterpFunc[qtm1Arg,rutm1Arg,0][[-1]]
}]

hmatApp03ValRec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath03ValsRecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]],Global`hmat.tp[[Range[9]+6]]]//Chop]


EndPackage[]
Print["done reading symb03ValsRec package"]







