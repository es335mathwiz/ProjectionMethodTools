BeginPackage["symb03ValsExp`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb03 package"]
(*compute z0 for time zero constraint only *)
Print["for three"]


aPath03ValsExp=genValsPath[03];


try03ValsExp={
((aPath03ValsExp[[5,1]]>=2/100&&Global`zzz$2$1[Global`t]==0)||
(aPath03ValsExp[[5,1]]==2/100&&Global`zzz$2$1[Global`t]>=0))&&
((aPath03ValsExp[[8,1]]>=2/100&&Global`zzz$1$1[Global`t]==0)||
(aPath03ValsExp[[8,1]]==2/100&&Global`zzz$1$1[Global`t]>=0))&&
((aPath03ValsExp[[11,1]]>=2/100&&Global`zzz$0$1[Global`t]==0)||
(aPath03ValsExp[[11,1]]==2/100&&Global`zzz$0$1[Global`t]>=0))
}







notVars03=Cases[Variables[Level[try03ValsExp,{-2}]],_[Global`t]]
numVars=Length[notVars03]/2;
epsVars03=notVars03[[Range[numVars]]]
zzzVars03=notVars03[[numVars+Range[numVars]]]



try03ValsExpFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try03ValsExp}/.epsVars03[[-1]]->0
try03ValsExpVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
NExpectation[zzzVars03/.solnCondsToPiecewise[Solve[
try03ValsExpFunc[Global`qtm1,Global`rutm1,Global`eps],zzzVars03,Reals]],
Drop[epsVars03,-1] \[Distributed] 
(MultinormalDistribution[ConstantArray[0,{1,numVars-1}],
Global`sigma$u*IdentityMatrix[numVars-1]]/.Global`lucaSubs)(*,PrecisionGoal->1,AccuracyGoal->1*)]


try03ValsExpq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath03ValsExp[[{7,9},1]]/.Flatten[Solve[try03ValsExpFunc[qtm1Arg,rutm1Arg,epsArg],
{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]}]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath03ValsExpExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[3,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try03ValsExpVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{Global`zzz$0$1[Global`t]:>tVals[[1]],
Global`zzz$1$1[Global`t]:>tVals[[2]],
Global`zzz$2$1[Global`t]:>tVals[[3]]
}]

hmatApp03ValsExp[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath03ValsExpExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
Print["trying 03computation"]
Print["with stdev=", Global`sigma$u*IdentityMatrix[numVars-1]/.Global`lucaSubs]

{symb03ValsExpFirstSecs,igVar}=Timing[try03ValsExpVals[-.1,-.1,0]];
Print["done first 03 computation"]
{symb03ValsExpSecondSecs,igVar}=Timing[try03ValsExpVals[-.1,-.1,0]];
Print["done second 03 computation"]
Splice["symb03ValsExpSecs.mtex"]


EndPackage[]
Print["done reading symb03ValsExp package"]
