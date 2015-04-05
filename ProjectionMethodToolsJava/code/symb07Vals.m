BeginPackage["symb07Vals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb07 package"]
(*compute z0 for time zero constraint only *)
Print["for 07"]


aPath07Vals=genPath[07];


try07Vals={
((aPath07Vals[[5,1]]>=0.02&&Global`zzz$6$1[Global`t]==0)||
(aPath07Vals[[5,1]]==0.02&&Global`zzz$6$1[Global`t]>=0))&&
((aPath07Vals[[8,1]]>=0.02&&Global`zzz$5$1[Global`t]==0)||
(aPath07Vals[[8,1]]==0.02&&Global`zzz$5$1[Global`t]>=0))&&
((aPath07Vals[[11,1]]>=0.02&&Global`zzz$4$1[Global`t]==0)||
(aPath07Vals[[11,1]]==0.02&&Global`zzz$4$1[Global`t]>=0))&&
((aPath07Vals[[14,1]]>=0.02&&Global`zzz$3$1[Global`t]==0)||
(aPath07Vals[[14,1]]==0.02&&Global`zzz$3$1[Global`t]>=0))&&
((aPath07Vals[[17,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath07Vals[[17,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
((aPath07Vals[[20,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath07Vals[[20,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath07Vals[[23,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath07Vals[[23,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}


vars07=Cases[Variables[Level[try07Vals,{-2}]],_[Global`t]]


try07ValsFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try07Vals}
try07ValsVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars07/.Flatten[Solve[try07ValsFunc[Global`qtm1,Global`rutm1,Global`eps],vars07,Reals]]


try07Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath07Vals[[{7,9},1]]/.Flatten[Solve[try07ValsFunc[qtm1Arg,rutm1Arg,epsArg],
vars07]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath07ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[07,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try07ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars07->tVals]]

hmatApp07Vals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath07ValsExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb07ValsFirstSecs,igVar}=Timing[try07ValsVals[-.1,-.1,0]];
{symb07ValsSecondSecs,igVar}=Timing[try07ValsVals[-.1,-.1,0]];
Splice["symb07ValsSecs.mtex"]

EndPackage[]
Print["done reading symb07Vals package"]
