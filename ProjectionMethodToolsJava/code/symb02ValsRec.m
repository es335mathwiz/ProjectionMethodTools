BeginPackage["symb02ValsRec`",{"labDocPrep`","ProjectionInterface`","symb01ValsRec`"}]
Print["reading symb02ValsRec package"]
(*compute z0 for time zero constraint only *)
Print["for 02"]


aPath02ValsRec=genPath[02];


try02ValsRec={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1InterpFunc[Global`qq[Global`t],Global`ru[Global`t],0]
}

qrTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1InterpFunc[Global`qqTry,Global`ruTry,0]
}
qrPreTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1PreInterpFunc[Global`qqTry,Global`ruTry,0]
}



qrAccTry02ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}

preTry02ValsRec={
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`qq[Global`t]==aPath02ValsRec[[4,1]]&&
Global`ru[Global`t]==aPath02ValsRec[[6,1]]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1PreInterpFunc[Global`qq[Global`t],Global`ru[Global`t],0]
}
(*
try02ValsRec={
(Global`zzz$0$1[Global`t]>=0&&Global`zzz$1$1[Global`t]>=0)&&
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1InterpFunc[aPath02ValsRec[[4,1]],aPath02ValsRec[[6,1]],0]
}
*)
try02ValsRecFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},try02ValsRec}
try02ValsRecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[try02ValsRecFunc[Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]


qrTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrTry02ValsRec[Global`qqTry,Global`ruTry]}

qrTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

valRec02[theFunc_,
Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
FixedPoint[theFunc[#1[[1]],#1[[2]]][Global`qtm1,Global`rutm1,Global`eps]&,
{Global`qqTry,Global`ruTry}]

qrPreTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrPreTry02ValsRec[Global`qqTry,Global`ruTry]}

qrPreTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrPreTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

qrAccTry02ValsRecFunc[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},qrAccTry02ValsRec[Global`qqTry,Global`ruTry]}

qrAccTry02ValsRecVals[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ][Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[FullSimplify[qrAccTry02ValsRecFunc[Global`qqTry,Global`ruTry][Global`qtm1,Global`rutm1,Global`eps]],vars02,Reals]]

vars02Start0={#,.5}&/@vars02;

fmTry02ValsRecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=With[{guts=FullSimplify[
{{Global`zzz$1$1[Global`t]^2+Global`zzz$0$1[Global`t]^2,
try02ValsRecFunc[Global`qtm1,Global`rutm1,Global`eps]},vars02Start0(*,
EvaluationMonitor:>Identity[With[{boo={Global`qtm1,Global`rutm1,Global`eps,Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t]}},Print[boo];boo]]*)
}]},
Global`zzz$1$1[Global`t]/.(FindMinimum @@ guts)[[-1]]]


(*
fmPreTry02ValsRecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{guts=FullSimplify[{{Global`zzz$1$1[Global`t]^2,
preTry02ValsRecFunc[Global`qtm1,Global`rutm1,Global`eps]},vars02(*,
EvaluationMonitor:>Identity[With[{boo={Global`qtm1,Global`rutm1,Global`eps,Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t]}},Print[boo];boo]]*)}]},
Global`zzz$1$1[Global`t]/.(FindMinimum @@ guts)[[-1]]]



fmPreTry02ValsRecVals=Function[
{Global`nqtm1,Global`nrutm1,Global`neps},
With[{guts=FullSimplify[{{Global`zzz$1$1[Global`t]^2,
preTry02ValsRecFunc[Global`nqtm1,Global`nrutm1,Global`neps]},vars02(*,
EvaluationMonitor:>Identity[With[{boo={Global`nqtm1,Global`nrutm1,Global`neps,Global`zzz$0$1[Global`t],Global`zzz$1$1[Global`t]}},Print[boo];boo]]*)}]},
Global`zzz$1$1[Global`t]/.(FindMinimum @@ guts)[[-1]]]]



*)


huhTry02ValsRec=
{aPath02ValsRec[[5,1]],GE,0.02,Global`zzz$1$1[Global`t],EQ,0,
aPath02ValsRec[[5,1]],EQ,0.02,Global`zzz$1$1[Global`t],GE,0,
Global`zzz$0$1[Global`t],EQ,
Global`zzz$0$1InterpFunc[aPath02ValsRec[[4,1]],aPath02ValsRec[[6,1]],0]}
huhTry02ValsRecFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},huhTry02ValsRec}


(*
preTry02ValsRec={
(Global`zzz$0$1[Global`t]>=0&&Global`zzz$0$1[Global`t]>=0)&&
((aPath02ValsRec[[5,1]]>=0.02&&Global`zzz$1$1[Global`t]==0)||
(aPath02ValsRec[[5,1]]==0.02&&Global`zzz$1$1[Global`t]>=0))&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1PreInterpFunc[aPath02ValsRec[[4,1]],aPath02ValsRec[[6,1]],0]
}
*)
preTry02ValsRecFunc=Function @@ {{Global`qtm1,Global`rutm1,Global`eps},preTry02ValsRec}
preTry02ValsRecVals[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
vars02/.Flatten[NSolve[preTry02ValsRecFunc[Global`qtm1,Global`rutm1,Global`eps],vars02,Reals]]


(*
delFuncSub[(Global`xx_InterpolatingFunction|Global`xx_Function)]:=
Global`zzz$1$1[Global`t]->
((Function @@ {{Global`qtm1Arg,Global`rutm1Arg,Global`epsArg},
Global`xx[Global`qtm1Arg,Global`rutm1Arg,Global`epsArg] +Global`zzz$0$1InterpFunc[Global`qtm1Arg,Global`rutm1Arg,Global`epsArg]})[Global`qtm1,Global`rutm1,Global`eps])
*)

Global`delFuncSub[targ_Symbol,
(Global`del_InterpolatingFunction|Global`del_Function),
(Global`base_InterpolatingFunction|Global`base_Function)]:=
targ[Global`t]->
((Function @@ {{Global`qtm1Arg,Global`rutm1Arg,Global`epsArg},
Global`del[Global`qtm1Arg,Global`rutm1Arg,Global`epsArg] +
Global`base[Global`qtm1Arg,Global`rutm1Arg,Global`epsArg]})[Global`qtm1,Global`rutm1,Global`eps])




(*


Plot3D @@ {{fmTry02ValsRecVals[qq,ru,0]},{qq,-0.4,0.4},{ru,-.08,.08}}

Plot3D @@ {{fmPreTry02ValsRecVals[qq,ru,0]},{qq,-0.4,0.4},{ru,-.08,.08}}

*)

fixZ0[Global`qqVar_?NumberQ,Global`ruVar_?NumberQ,Global`epsVar_?NumberQ,
(Global`del_InterpolatingFunction|Global`del_Function),
(Global`base_InterpolatingFunction|Global`base_Function)]:=
With[{dSub=Global`delFuncSub[Global`zzz$1$1,
Global`del,
Global`base],
zImpct=(Global`phimat . Global`psiz)[[2,1]] },
With[{delZPred=-zImpct*(aPath02ValsRec[[5,1]]-0.02),
theZSubs=Append[Flatten[FindRoot[
(Global`zzz$0$1[Global`t]==
Global`base[aPath02ValsRec[[4,1]],aPath02ValsRec[[6,1]],0]/.
dSub/.{Global`qtm1->Global`qqVar,Global`rutm1->Global`ruVar,Global`eps->Global`epsVar}),
{Global`zzz$0$1[Global`t],0}]],dSub/.{Global`qtm1->Global`qqVar,Global`rutm1->Global`ruVar,Global`eps->Global`epsVar}]},
Join[({aPath02ValsRec[[5,1]],delZPred,aPath02ValsRec,
preTry02ValsRec,symb02`try02}/.theZSubs)/.
{Global`qtm1->Global`qqVar,
Global`rutm1->Global`ruVar,
Global`eps->Global`epsVar},
{symb02`try02ValsSoln[Global`qqVar,Global`ruVar,Global`epsVar],
theZSubs}]//N]]

updt[dVal_?NumberQ]:=
fixZ0[-.1,-.08,0,Function[{Global`x,Global`y,Global`z},dVal],Global`zzz$0$1PreInterpFunc][[2]]//N
(*
fixZ0[.1,.08,0,Function[{x,y,z},0],Global`zzz$0$1PreInterpFunc]
fixZ0[-.1,-.08,0,Function[{x,y,z},0],Global`zzz$0$1PreInterpFunc]
With[{cmp=symb02`try02ValsSoln[-.1,-.08,0]},
fixZ0[-.1,-.08,0,Function[{x,y,z},cmp[[2]]-cmp[[1]]],Global`zzz$0$1PreInterpFunc]]
*)
(*Global`zzz$0$1InterpFunc[Global`qq[Global`t],Global`ru[Global`t],0]*)

vars02=Cases[Variables[Level[try02ValsRec,{-2}]],_[Global`t]]


(*

try02ValsRecq1r1Vals[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
(aPath02ValsRec[[{7,9},1]]/.Flatten[Solve[try02ValsRecFunc[qtm1Arg,rutm1Arg,epsArg],
vars02]])/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg}

aPath02ValsRecExtFunc[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=  
With[{tp=genPath[02,1]/.{Global`qtm1->qtm1Arg,Global`rutm1->rutm1Arg,Global`eps->epsArg},
tVals=try02ValsRecVals[qtm1Arg,rutm1Arg,epsArg]},
tp/.Thread[vars02->tVals]]

hmatApp02ValsRec[Global`qtm1_?NumberQ,Global`rutm1_?NumberQ,Global`eps_?NumberQ]:=
With[{tp=aPath02ValsRecExtFunc[Global`qtm1,Global`rutm1,Global`eps]},
Join[Global`hmat.tp[[Range[9]]],
Global`hmat.tp[[Range[9]+3]],
Global`hmat.tp[[Range[9]+6]]
]//Chop]
(*
igVar=Unique[];
Print["symb02ValsRec first timing"]
{symb02ValsRecFirstSecs,igVar}=Timing[try02ValsRecVals[-.1,-.1,0]];
Print["symb02ValsRec second timing"]
{symb02ValsRecSecondSecs,igVar}=Timing[try02ValsRecVals[-.1,-.1,0]];
Splice["symb02ValsRecSecs.mtex"]

*)
Print["construct pre interp functions"]
Global`zzz$1$1PreInterpFunc=Function[{Global`qqVar,Global`ruVar,Global`epsVar},
(Global`zzz$0$1[Global`t]+Global`delZ)/.Flatten[
Solve[try02ValsRec/.Global`zzz$1$1[Global`t]->(Global`delZ+Global`zzz$0$1InterpFunc[Global`qtm1,Global`rutm1,Global`eps])/.
{Global`qtm1->Global`qqVar,Global`rutm1->Global`ruVar,Global`eps->Global`epsVar},{Global`zzz$0$1[Global`t],Global`delZ},Reals]]]

Print["construct pre interp functions"]
Global`chkZzz$1$1PreInterpFunc=Function[{Global`qqVar,Global`ruVar,Global`epsVar},
(Global`zzz$0$1[Global`t]+Global`delZ)/.Flatten[
Solve[try02ValsRec/.Global`zzz$1$1[Global`t]->(Global`delZ+Global`zzz$0$1PreInterpFunc[Global`qtm1,Global`rutm1,Global`eps])/.
{Global`qtm1->Global`qqVar,Global`rutm1->Global`ruVar,Global`eps->Global`epsVar},{Global`zzz$0$1[Global`t],Global`delZ},Reals]]]



Print["construct interpolation"]
Global`zzz$1$1InterpFunc=Global`makeInterpFunc[fmPreTry02ValsRecVals];


*)


Print["construct more accurate interpolation"]
theOrd=4;thePts=100;
{interpTime02ValsRec,ig}=Timing[Global`zzz$0$1AccInterpFunc=Global`makeInterpFunc[Global`zzz$0$1PreInterpFunc,theOrd,thePts]];
igVar=Unique[];
{symb02ValsRecFirstSecs,igVar}=Timing[
valRec02[qrAccTry02ValsRecVals,.0341293,.015][-.1,-.08,.01]];
{symb02ValsRecSecondSecs,igVar}=Timing[
valRec02[qrAccTry02ValsRecVals,.0341293,.015][-.1,-.08,.01]];
Splice["symb02ValsSecs.mtex"]


EndPackage[]
Print["done reading symb02ValsRec package"]







