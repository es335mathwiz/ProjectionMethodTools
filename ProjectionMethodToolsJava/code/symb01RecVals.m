BeginPackage["symb01RecVals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb01 package"]

(*compute z0 for time zero constraint only *)

Print["for one"]

aPath01RecVals=genPath[1]

try01RecVals={
(aPath01RecVals[[5,1]]>=0.02&&Global`zzz$0$1[Global`t]==0)||
(aPath01RecVals[[5,1]]==0.02&&Global`zzz$0$1[Global`t]>=0)
}
Export["try01RecValsA.pdf",try01RecVals[[1,1]]]
Export["try01RecValsB.pdf",try01RecVals[[1,2]]]

try01RecValsFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
{Global`zzz$0$1[Global`t]}/.
Flatten[Solve[
try01RecVals/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},{Global`zzz$0$1[Global`t]},Reals]]
igVar=Unique[];


EndPackage[]
Print["done reading symb01RecVals package"]
