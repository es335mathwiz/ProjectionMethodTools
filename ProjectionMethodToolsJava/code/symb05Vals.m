BeginPackage["symb05Vals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb05 package"]
(*compute z0 for time zero constraint only *)
Print["for 05"]


aPath05Vals=genPath[05];


try05Vals={
((aPath05Vals[[5,1]]>=0.02&&Global`zzz$4$1[Global`t]==0)||
(aPath05Vals[[5,1]]==0.02&&Global`zzz$4$1[Global`t]>=0))&&
((aPath05Vals[[8,1]]>=0.02&&Global`zzz$3$1[Global`t]==0)||
(aPath05Vals[[8,1]]==0.02&&Global`zzz$3$1[Global`t]>=0))&&
((aPath05Vals[[11,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath05Vals[[11,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
((aPath05Vals[[14,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath05Vals[[14,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath05Vals[[17,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath05Vals[[17,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}


vars05=Cases[Variables[Level[try05Vals,{-2}]],_[Global`t]]


try05ValsFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try05Vals}
try05ValsVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars05/.Flatten[Solve[try05ValsFunc[Global`qtm1,Global`rutm1,Global`eps],vars05,Reals]]


try05Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath05Vals[[{7,9},1]]/.Flatten[Solve[try05ValsFunc[qtm1Arg,rutm1Arg,epsArg],
vars05]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath05ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[3,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try05ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars05->tVals]]

hmatApp05Vals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath05ValsExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb05ValsFirstSecs,igVar}=Timing[try05ValsVals[-.1,-.1,0]];
{symb05ValsSecondSecs,igVar}=Timing[try05ValsVals[-.1,-.1,0]];
Splice["symb05ValsSecs.mtex"]

EndPackage[]
Print["done reading symb05Vals package"]
