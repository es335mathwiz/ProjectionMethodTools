Needs["occBindRecur`"]



tryAssess=Table[Timing[assessPF[iOrd,,z01ExactInitPF,2]]


(*
theMaxNesting=2;theMaxOrder=1;theMaxPts=3;
(*
compsPFNull=Table[forIOrdNPtsPF[ii,jj,{},theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
*)


huh01$01=forIOrdNPtsPF[0,1,{},1]
fpForInitStateFunc[-.4,0,0,huh01$01[[-1,-1]]]

huh10$01=forIOrdNPtsPF[0,10,{},1]
fpForInitStateFunc[-.4,0,0,huh10$01[[-1,-1]]]
huh20$01=forIOrdNPtsPF[0,20,{},1]
fpForInitStateFunc[-.4,0,0,huh20$01[[-1,-1]]]


huh01$02=forIOrdNPtsPF[0,1,{},2]
fpForInitStateFunc[-.4,0,0,huh01$02[[-1,-1]]]

huh10$02=forIOrdNPtsPF[0,10,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]

huh01$01E=forIOrdNPtsPF[0,1,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh01$01E[[-1,-1]]]

huh10$01E=forIOrdNPtsPF[0,10,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh10$01E[[-1,-1]]]


huh10$01E2=forIOrdNPtsPF[2,10,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh10$01E2[[-1,-1]]]

huh10$02=forIOrdNPtsPF[2,20,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]




trynow=Function[{qq,ru,eps}, Max[Abs[
hmatApp[qq,ru,eps,huh[[-1,-1]]]]]]

infNorm[trynow]


hey=Function[
{ww,xx,yy,zz},
With[{huhxx=forIOrdNPtsPF[ww,xx,{},1]},
Transpose[{Through[huhxx[[-1,-1]][yy,zz]],Through[z01ExactInitPF[yy,zz]]}]]]


xx=1;yy=5;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,2]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]


xx=1;yy=5;
huh10$01E2n=forIOrdNPtsPF[xx,yy,{},2]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1]]]
simPFPath[2,-.4,-.020,-.01,huh10$01E2n]
fpForInitStateFunc[-.4,0,0,huh10$01E2[[-1,-1]]]




xx=1;yy=15;
huh10$01E2n=forIOrdNPtsPF[xx,yy,{},3]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[-.4,-.020,-.01,huh10$01E2n]





xx=1;yy=10;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,3]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[-.4,-.020,-.01,huh10$01E2n]



xx=1;yy=10;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,1];
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[-.4,-.020,-.01,huh10$01E2n]
boop=Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,huh10$01E2n[[-1,-1]]]];
donow=makeInterpFuncFinal[boop,Range[5],xx,yy,{-.4,.4},{-.08,.08},{-.01,.01}];
simPathFinal[-.4,-.020,-.01,donow]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
aPathFinal[-.4,-.020,-.01,donow]
hmatAppFinal[-.4,-.020,-.01,donow]
smallestRVal[donow]
assessPF[1,4,z01ExactInitPF]

trynow=Function[{qq,ru,eps}, Max[Abs[hmatAppFinal[qq,ru,eps,donow]]]]
infNorm[trynow]

(*
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]],0]
]

Out[25]= {{-0.4}, {rtm1}, {-0.02}, {-0.489917}, {0.02}, {-0.02}, {-0.504884}, 
 
>    {0.02}, {-0.01}, {-0.464495}, {0.02}, {-0.005}, {-0.377885}, {0.02}, 
 
>    {-0.0025}, {-0.248762}, {0.02}, {-0.00125}, {-0.0779192}, {-0.0629192}, 
 
>    {-0.000625}}

huh10$02=forIOrdNPtsPF[1,10,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]

iterateDR::usage="iterateDR[drFunc_Function,initVec:{initQ_?NumberQ,initRu_?NumberQ,initEps_?NumberQ},numPers_Integer,stdev_?NumberQ]"



whole=Table[genFinalPF[1,10,z01ExactInitPF,ii],{ii,1,2}]
trypf=Function[{qq,ru,eps},
Flatten[aPathFinal[qq,ru,eps,whole[[-1,-1]]][[3+Range[3]]]]]
{tmpf,tsdpf}=iterateDR[trypf,{-.4,0,0},sigma$u//.lucaSubs,2,40]
hmat . tmpf[[Range[9]]]  -psic//numIt

wholeRE=Table[genFinalRE[1,10,z01ExactInitRE,ii],{ii,1,2}]
tryre=Function[{qq,ru,eps},
Flatten[aPathFinal[qq,ru,eps,wholeRE[[-1,-1]]][[3+Range[3]]]]]
{tmre,tsdre}=iterateDR[tryre,{-.4,0,0},sigma$u//.lucaSubs,2,40]
hmat . tmre[[Range[9]]]  -psic//numIt


notassessPF[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]:=
With[{zFuncs=forIOrdNPtsPF[iOrd,nPts,initFuncs,iters]},
With[{preInterpFunc=
Function[{qq,ru,eps},fpForInitStateFunc[qq,ru,eps,zFuncs[[-1]]]]},
With[{numVals=Length[preInterpFunc[0,0,0]]},
With[{interpFuncFinal=
makeInterpFuncFinal[preInterpFunc,Range[numVals],
iOrd,nPts,
({qLow,qHigh}//.lucaSubs)//N,
({ruLow,ruHigh}//.lucaSubs)//N,
({-2*sigma$u,2*sigma$u}//.lucaSubs)//N]},
{zFuncs,preInterpFunc,interpFuncFinal}]]]]



*)




(*

{{iOrd,nPts},{},zFuncs,interpFuncFinal,smallestRVal[interpFuncFinal],infNormFinal[interpFuncFinal]}]]]]*)



xx=1;yy=3;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,1];
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[-.4,-.020,-.01,huh10$01E2n]
boop=Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,huh10$01E2n[[-1,-1]]]];
donow=makeInterpFuncFinal[boop,Range[5],xx,yy,{-.4,.4},{-.08,.08},{-.01,.01}];
simPathFinal[-.4,-.020,-.01,donow]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
aPathFinal[-.4,-.020,-.01,donow]
hmatAppFinal[-.4,-.020,-.01,donow]
smallestRVal[donow]
assessPF[1,4,z01ExactInitPF]



wholeRE=Table[genFinalRE[1,10,{},ii],{ii,1,2}]
tryre=Function[{qq,ru,eps},
Flatten[aPathFinal[qq,ru,eps,wholeRE[[-1,-1]]][[3+Range[3]]]]]
{tmre,tsdre}=iterateDR[tryre,{-.4,0,0},2,sigma$u//.lucaSubs]



wholePF=Table[genFinalPF[1,10,{},ii],{ii,1,2}]
tryre=Function[{qq,ru,eps},
Flatten[aPathFinal[qq,ru,eps,wholePF[[-1,-1]]][[3+Range[3]]]]]
{tmpf,tsdpf}=iterateDR[tryre,{-.4,0,0},2,sigma$u//.lucaSubs]


hmat . tmpf[[Range[9]]]  -psic//numIt

hmat . tmre[[Range[9]]]  -psic//numIt


*)
