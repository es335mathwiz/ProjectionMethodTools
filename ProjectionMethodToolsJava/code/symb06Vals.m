BeginPackage["symb06Vals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb06 package"]
(*compute z0 for time zero constraint only *)
Print["for 06"]


aPath06Vals=genPath[06];


try06Vals={
((aPath06Vals[[5,1]]>=0.02&&Global`zzz$5$1[Global`t]==0)||
(aPath06Vals[[5,1]]==0.02&&Global`zzz$5$1[Global`t]>=0))&&
((aPath06Vals[[8,1]]>=0.02&&Global`zzz$4$1[Global`t]==0)||
(aPath06Vals[[8,1]]==0.02&&Global`zzz$4$1[Global`t]>=0))&&
((aPath06Vals[[11,1]]>=0.02&&Global`zzz$3$1[Global`t]==0)||
(aPath06Vals[[11,1]]==0.02&&Global`zzz$3$1[Global`t]>=0))&&
((aPath06Vals[[14,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath06Vals[[14,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
((aPath06Vals[[17,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath06Vals[[17,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath06Vals[[20,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath06Vals[[20,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}


vars06=Cases[Variables[Level[try06Vals,{-2}]],_[Global`t]]


try06ValsFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try06Vals}
try06ValsVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars06/.Flatten[Solve[try06ValsFunc[Global`qtm1,Global`rutm1,Global`eps],vars06,Reals]]


try06Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath06Vals[[{7,9},1]]/.Flatten[Solve[try06ValsFunc[qtm1Arg,rutm1Arg,epsArg],
vars06]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath06ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[06,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try06ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars06->tVals]]

hmatApp06Vals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath06ValsExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb06ValsFirstSecs,igVar}=Timing[try06ValsVals[-.1,-.1,0]];
{symb06ValsSecondSecs,igVar}=Timing[try06ValsVals[-.1,-.1,0]];
Splice["symb06ValsSecs.mtex"]

EndPackage[]
Print["done reading symb06Vals package"]
