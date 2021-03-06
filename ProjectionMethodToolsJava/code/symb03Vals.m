BeginPackage["symb03Vals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb03 package"]
(*compute z0 for time zero constraint only *)
Print["for three"]


aPath03Vals=genPath[3];


try03Vals={
((aPath03Vals[[5,1]]>=0.02&&Global`zzz$2$1[Global`t]==0)||
(aPath03Vals[[5,1]]==0.02&&Global`zzz$2$1[Global`t]>=0))&&
((aPath03Vals[[8,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath03Vals[[8,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
((aPath03Vals[[11,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath03Vals[[11,1]]==0.02&&Global`zzz$0$1[Global`t]>=0))
}

try03ValsFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try03Vals}
try03ValsVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
{Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$2$1[Global`t]}/.Flatten[Solve[try03ValsFunc[Global`qtm1,Global`rutm1,Global`eps],
{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]}]]


try03Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath03Vals[[{7,9},1]]/.Flatten[Solve[try03ValsFunc[qtm1Arg,rutm1Arg,epsArg],
{Global`zzz$2$1[Global`t],Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t]}]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath03ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[3,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try03ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.{Global`zzz$0$1[Global`t]:>tVals[[1]],
Global`zzz$1$1[Global`t]:>tVals[[2]],
Global`zzz$2$1[Global`t]:>tVals[[3]]
}]

hmatApp03Vals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath03ValsExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb03ValsFirstSecs,igVar}=Timing[try03ValsVals[-.1,-.1,0]];
{symb03ValsSecondSecs,igVar}=Timing[try03ValsVals[-.1,-.1,0]];
Splice["symb03ValsSecs.mtex"]

EndPackage[]
Print["done reading symb03Vals package"]
