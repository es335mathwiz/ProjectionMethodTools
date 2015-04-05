BeginPackage["symb02ValsExp`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb02 package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]


aPath02ValsExp=genValsPath[02];


try02ValsExp={
((aPath02ValsExp[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsExp[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath02ValsExp[[8,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath02ValsExp[[8,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}







notVars02=Cases[Variables[Level[try02ValsExp,{-2}]],_[Global`t]]
numVars=Length[notVars02]/2;
epsVars02=notVars02[[Range[numVars]]]
zzzVars02=notVars02[[numVars+Range[numVars]]]

try02ValsExpFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02ValsExp/.epsVars02[[-1]]->0}
try02ValsExpVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
NExpectation[zzzVars02/.solnCondsToPiecewise[Solve[
try02ValsExpFunc[Global`qtm1,Global`rutm1,Global`eps],zzzVars02,Reals]],
Global`epsVar$0$1[Global`t] \[Distributed] (NormalDistribution[0,Global`sigma$u]/.Global`lucaSubs)(*,PrecisionGoal->1,AccuracyGoal->1*)]


try02ValsExpq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath02ValsExp[[{7,9},1]]/.Flatten[Solve[try02ValsExpFunc[qtm1Arg,rutm1Arg,epsArg],
zzzVars02]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath02ValsExpExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[02,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try02ValsExpVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[zzzVars02->tVals]]

hmatApp02ValsExp[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath02ValsExpExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
Print["trying 02computation"]
Print["with stdev=", Global`sigma$u*IdentityMatrix[numVars-1]/.Global`lucaSubs]
{symb02ValsExpFirstSecs,igVar}=Timing[try02ValsExpVals[-.1,-.1,0]];
Print["doing second time"]
{symb02ValsExpSecondSecs,igVar}=Timing[try02ValsExpVals[-.1,-.1,0]];
Splice["symb02ValsExpSecs.mtex"]

EndPackage[]
Print["done reading symb02ValsExp package"]
