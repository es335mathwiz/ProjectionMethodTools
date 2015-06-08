BeginPackage["symb05ValsRec`",{"labDocPrep`","ProjectionInterface`","symb04ValsRec`"}]
Print["reading symb05ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 05"]

aPath05ValsRec=genPath[05];
(*needs explict mod here*)
qrAccTry05ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath05ValsRec[[5,1]]>=0.02&&Global`zzz$4$1[Global`t]==0)||(*here*)
(aPath05ValsRec[[5,1]]==0.02&&Global`zzz$4$1[Global`t]>=0))&&(*here*)
Global`qq[Global`t]==aPath05ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath05ValsRec[[6,1]]&&
Global`zzz$3$1[Global`t]==(*add eqn here*)
Global`zzz$3$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$2$1[Global`t]==
Global`zzz$2$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$1$1[Global`t]==
Global`zzz$1$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}

vars05=Cases[Variables[Level[qrAccTry05ValsRec[0,0],{-2}]],_[Global`t]]


(*needs explicit mod here*)
igVar=Unique[];
{interpTime05ValsRec,igVar}=
Timing[{Global`zzz$0$1AccInterpFunc,Global`zzz$1$1AccInterpFunc,Global`zzz$2$1AccInterpFunc,Global`zzz$3$1AccInterpFunc}=Global`makeInterpFunc[
Global`zzz$3$1PreInterpFunc,{-4,-3,-2,-1},(*and here*)
Global`theOrd,Global`thePts,
{Global`qlv,Global`qhv},
{Global`qlv,Global`qhv},
{Global`elv,Global`ehv}]];


qrAccTry05ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry05ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry05ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars05/.Flatten[NSolve[Identity[qrAccTry05ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars05,Reals]]

Print["construct more accurate interpolation"]
Global`zzz$4$1PreInterpFunc=With[{proc=Global`valRecN[qrAccTry05ValsRecVals]},
Function[{Global`qq,Global`ru,Global`eps},proc[Global`qq,Global`ru,Global`eps]]]

Print["prepare for splicing 05"]
{symb05ValsRecFirstSecs,igVar}=Timing[
Global`valRecN[qrAccTry05ValsRecVals][-.1,-.08,.01]];
{symb05ValsRecSecondSecs,igVar}=Timing[
Global`valRecN[qrAccTry05ValsRecVals][-.1,-.08,.01]];
Splice["symb05ValsRecSecs.mtex"]


aPath05ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[05,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},qrVals=valRecN[qrAccTry05ValsRecVals][qtm1Arg,rutm1Arg,epsArg],
tVals=try05ValRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{
Global`zzz$0$1[Global`t]:>
		Global`zzz$0$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$1$1[Global`t]:>
		Global`zzz$1$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$2$1[Global`t]:>
		Global`zzz$2$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$3$1[Global`t]:>
		Global`zzz$3$1AccInterpFunc[qrVals[[1]],qrVals[[2]],0],
Global`zzz$4$1[Global`t]:>
		Global`zzz$4$1PreInterpFunc[qtm1Arg,rutm1Arg,0][[-1]]
}]

hmatApp05ValRec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath05ValsRecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],Global`hmat.tp[[Range[9]+3]],Global`hmat.tp[[Range[9]+6]]]//Chop]


EndPackage[]
Print["done reading symb05ValsRec package"]







