BeginPackage["symb02Vals`",{"occBindRecur`","ProtectedSymbols`","ProjectionInterface`"}]
Print["reading symb02 package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]
Print["takes less than minute"]


aPath02Vals=Private`genPath[02];


try02Vals={
((aPath02Vals[[5,1]]>=0.02&&zzz$1$1[t]==0)||
(aPath02Vals[[5,1]]==0.02&&zzz$1$1[t]>=0))&&
((aPath02Vals[[8,1]]>=0.02&&zzz$0$1[t]==0)||
(aPath02Vals[[8,1]]==0.02&&zzz$0$1[t]>=0))
}


vars02=Cases[Variables[Level[try02Vals,{-2}]],_[t]]


try02ValsFunc=Function @@ {{qtm1,rutm1,eps},try02Vals}
try02ValsVals[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
vars02/.Flatten[Solve[try02ValsFunc[qtm1,rutm1,eps],vars02,Reals]]


try02Valsq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath02Vals[[{7,9},1]]/.Flatten[Solve[try02ValsFunc[qtm1Arg,rutm1Arg,epsArg],
vars02]])/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg}

aPath02ValsExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=Private`genPath[02,1]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},
tVals=try02ValsVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars02->tVals]]

hmatApp02Vals[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ]:=
With[{tp=aPath02ValsExtFunc[qtm1,rutm1,eps]},
Join[hmat.tp[[Range[9]]],
hmat.tp[[Range[9]+3]],
hmat.tp[[Range[9]+6]]
]//Chop]

igVar=Unique[];
{symb02ValsFirstSecs,igVar}=Timing[try02ValsVals[-.1,-.1,0]];
{symb02ValsSecondSecs,igVar}=Timing[try02ValsVals[-.1,-.1,0]];
Splice["symb02ValsSecs.mtex"]

EndPackage[]
Print["done reading symb02Vals package"]
