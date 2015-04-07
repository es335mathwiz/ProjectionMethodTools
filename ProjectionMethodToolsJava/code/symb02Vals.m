BeginPackage["symb02Vals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb02 package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]
Print["takes less than minute"]


aPath02Vals=genPath[02];


try02Vals={
((aPath02Vals[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02Vals[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath02Vals[[8,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath02Vals[[8,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}


vars02=Cases[Variables[Level[try02Vals,{-2}]],_[Global`t]]


try02ValsFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02Vals}
try02ValsVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[Solve[try02ValsFunc[Global`qtm1,Global`rutm1,Global`eps],vars02,Reals]]


try02Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath02Vals[[{7,9},1]]/.Flatten[Solve[try02ValsFunc[qtm1Arg,rutm1Arg,epsArg],
vars02]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath02ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[02,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try02ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars02->tVals]]

hmatApp02Vals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath02ValsExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb02ValsFirstSecs,igVar}=Timing[try02ValsVals[-.1,-.1,0]];
{symb02ValsSecondSecs,igVar}=Timing[try02ValsVals[-.1,-.1,0]];
Splice["symb02ValsSecs.mtex"]

EndPackage[]
Print["done reading symb02Vals package"]
