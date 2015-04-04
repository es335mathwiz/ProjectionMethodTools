BeginPackage["symb02RecVals`",{"labDocPrep`","ProjectionInterface`"}]
Print["reading symb01 package"]

(*compute z0 for time zero constraint only *)

Print["for two"]

aPath02RecVals=genPath[2]

try02RecVals={
((aPath02RecVals[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02RecVals[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02RecVals[[4,1]]&&
Global`ru[Global`t]==aPath02RecVals[[6,1]]&&
Global`zzz$0$1[Global`t]==try01RecValsFunc[Global`qq[Global`t],Global`ru[Global`t],0]
}
Export["try02RecValsA.pdf",try02RecVals[[1,1]]]
Export["try02RecValsB.pdf",try02RecVals[[1,2]]]

try02RecValsFunc[qtm1Arg_?NumberQ,Global`rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
{Global`zzz$0$1[Global`t]}/.
Flatten[NSolve[
try02RecVals/.{Global`qtm1->qtm1Arg,Global`rutm1->Global`rutm1Arg,Global`eps->epsArg},
{Global`zzz$1$1[Global`t],Global`zzz$0$1[Global`t],Global`qq[Global`t],Global`ru[Global`t]},Reals]]
igVar=Unique[];


EndPackage[]
Print["done reading symb02RecVals package"]


