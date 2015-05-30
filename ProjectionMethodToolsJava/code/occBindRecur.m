PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
Print["reading occBindRecur.m"]
BeginPackage["occBindRecur`",{"ProtectedSymbols`","ProjectionInterface`","JLink`","AMAModel`","NumericAMA`"}]
simPFPath::usage="simPFPath[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"
getNextPt::usage="getNextPt[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"
iterPF::usage="iterPF[iorder,numpts,zFuncsNow_List]"
iterRE::usage="iterRE[iorder,numpts,zFuncsNow_List]"
aPath::usage="aPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"  
hmatApp::usage="hmatApp[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,zFuncs_List]"
infNorm::usage="infNorm[func_]"
assessLatestPF::usage="assessLatestPF[zFuncs_List]"
z01ExactInitRE::usage="exact ratex for one period"
z01ExactInitPF::usage="exact perfect foresight for one period"




genCompSlackSysFunc::usage="genCompSlack[pathLen_Integer,zFuncs:{_Function...}]"
fpForInitStateFunc::usage="fpForInitVecFunc[qVal,ruVal,epsVal,theCompSlackSysFunc_,zFuncs:{_Function...}]"
makeInterpFuncPF::usage="makeInterpFunc";
makeInterpFuncRE::usage="makeInterpFunc";
numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ,fmat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"
forIOrdNPtsPF::usage="forIOrdNPtsPF[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
forIOrdNPtsRE::usage="forIOrdNPtsRE[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
doChkLoad::usage="doChkLoad[]"


lucaEqns::usage="simple model equations"
lucaSubs::usage="typical simple model parameters"

qq::usage="variable for model"
rr::usage="variable for model"
ru::usage="variable for model"
qtm1::usage="variable for model"
rtm1::usage="variable for model"
rutm1::usage="variable for model"
rhop::usage="parameter for model"
betap::usage="parameter for model"
sigmap::usage="parameter for model"
sigma$u::usage="parameter for model"
rstar::usage="parameter for model"
rho$ru::usage="parameter for model"
adj::usage="parameter for model"
uu::usage="parameter for model"
phip::usage="parameter for model"
rUnderBar::usage="parameter for model"
Protect[qq,rr,ru,rhop,betap,sigmap,rho$ru,adj,uu,phip,rUnderBar,sigma$u,rstar]

(*assigned Private`ly below*)
hmat::usage="simple model matrix"
qmat::usage="simple model matrix"
bmat::usage="simple model matrix"
phimat::usage="simple model matrix"
fmat::usage="simple model matrix"
psiz::usage="simple model matrix"
psic::usage="simple model matrix"
psieps::usage="simple model matrix"
noCnstrnGuess::usage="no constraints guess lin system computation for q and ru"


numIt::usage="numIt[xx_]  does lucaSubs and leaves t values untouched by N[]"






Begin["Private`"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]

Print["occBindRecur: Turning off extrapolation warning messages"]
Off[InterpolatingFunction::dmval];


numIt[xx_]:=xx//.lucaSubs//myN//Expand//Chop 
 
forIOrdNPtsPF[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]:=
NestList[Timing[iterPF[iOrd,nPts,#[[2]]]]&,{{},start},maxLen];

forIOrdNPtsRE[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]:=
NestList[Timing[iterRE[iOrd,nPts,#[[2]]]]&,{{},start},maxLen];
doChkLoad[]:=
Module[{},
Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"];
Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"];
Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"];
{numProcs,loadAvg,freeMem}//ToString//ToExpression]


assessLatestPF[zFuncs_List]:=
With[{delFunc=
Function[{xx,yy,zz},With[{fir=fpForInitStateFunc[xx,yy,zz,zFuncs[[-1]],1],
sec=fpForInitStateFunc[xx,yy,zz,zFuncs[[-2]],1]},
fir-sec]]},
delFunc]



genCompSlackSysFunc[pathLen_Integer]:=
With[{aPath=genPath[pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{compCon=((aPath[[5,1]]>=(rUnderBar/.lucaSubs)&&theZs[[-1]]==0)||
(aPath[[5,1]]==(rUnderBar/.lucaSubs)&&theZs[[-1]]>=0)),
rhsEqns={aPath[[4,1]],aPath[[6,1]]}},
With[{zLeft=(Drop[theZs,-1])},
With[{theSys=And[compCon,zEqns]},
{compCon,rhsEqns}]]]]/;
And[pathLen>0]



fpForInitStateFunc[qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
zFuncs_List,(pos_List)|(pos_Integer)]:=
With[{beenDone=fpForInitStateFunc[qVal,ruVal,epsVal,zFuncs]},
beenDone[[pos]]]


fpForInitStateFunc[qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
zFuncs_List]:=
fpForInitStateFunc[qVal,ruVal,epsVal,zFuncs]=
With[{pathLen=If[zFuncs==={},1,Length[zFuncs]-1],
valSubs={qtm1->qVal,rutm1->ruVal,eps->epsVal}},
With[{csrhs=genCompSlackSysFunc[pathLen]/.valSubs,
initGuess=If[Length[zFuncs]==0,
noCnstrnGuess[qVal,ruVal][[{1,2}]],
{zFuncs[[1]][qVal,ruVal],zFuncs[[2]][qVal,ruVal]}],
aPath=genPath[pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{initStateSubbed=csrhs[[1]],
tryEqnsSubbed=And @@Thread[{qTry,rTry}==(csrhs[[2]])]},
With[{zLeft=(Drop[theZs,-1])},
With[{theSys=Function[{qTry,rTry},
With[{zFuncsApps=If[pathLen===1,{},Through[Drop[zFuncs,2][qTry,rTry]]]},
With[{zEqns=And @@ (Thread[zLeft==zFuncsApps])},
And[initStateSubbed,zEqns,tryEqnsSubbed]]]]},
With[{zVars=Union[Cases[initStateSubbed,xx_[t],Infinity]]},
With[{fpTarget=Join[{qTry,rTry},theZs]},
If[Drop[Union[fpTarget],2]=!=Union[theZs],Print["diff zs",Drop[Union[fpTarget],2],Union[theZs],zEqns]];
FixedPoint[fpTarget/.With[{soln=
Flatten[NSolve[theSys @@ #,fpTarget]]},
If[Not[MatchQ[soln,{(_->_)..}]],Throw[{"NSolve Failed in fpForInitState for",{theSys,fpTarget}}],soln]]&,initGuess]]]]]]]]




makeInterpFuncPF[theFunc_Function,pos_List,iOrder_Integer,iPts_Integer,
{qLow_?NumberQ,qHigh_?NumberQ},
{ruLow_?NumberQ,ruHigh_?NumberQ}
]:=Module[{thePts=
gridPts[iPts,
{qLow,qHigh},
{ruLow,ruHigh}],
pfFunc=Function[{qq,ru},theFunc[qq,ru,0]]},
With[{whl={#,pfFunc @@ #}& /@
thePts},
Sow[theFunc];
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{theRes=theFunc[0,0,0]},Print["iPts:theRes=",theRes];
NumberQ[Plus @@ theRes[[pos]]]]

makeInterpFuncPF[theFunc_Function,iOrder_Integer,iPts_Integer,
{qLow_?NumberQ,qHigh_?NumberQ},
{ruLow_?NumberQ,ruHigh_?NumberQ}
]:=With[{pos=Range[Length[theFunc[0,0,0]]]},
makeInterpFuncPF[theFunc,pos,iOrder,iPts,
{qLow,qHigh},
{ruLow,ruHigh}]]


makeInterpFuncRE[theFunc_Function,iOrder_Integer,iPts_Integer,
{qLow_?NumberQ,qHigh_?NumberQ},
{ruLow_?NumberQ,ruHigh_?NumberQ}]:=
With[{theRange=Range[Max[2,Length[theFunc[[2,4]]]]+1]},
makeInterpFuncRE[theFunc,#,iOrder,iPts,
{qLow,qHigh},
{ruLow,ruHigh}]&/@theRange]






makeInterpFuncRE[theFunc_Function,pos_Integer,iOrder_Integer,iPts_Integer,
{qLow_?NumberQ,qHigh_?NumberQ},
{ruLow_?NumberQ,ruHigh_?NumberQ}
]:=Module[{thePts=
gridPts[iPts,
{qLow,qHigh},
{ruLow,ruHigh}],
reFunc=Function @@ {{qq,ru},
With[{qrSubbed=theFunc[qq,ru,#,thePos]},
myExpect[Sow[Identity[With[{hoop=(qrSubbed&[tryEps])*PDF[NormalDistribution[0,sigma$u],
tryEps]/.lucaSubs},hoop]]],tryEps]]}},
With[{whl={#,reFunc @@ #}& /@
thePts},
Sow[theFunc];
doScalarIntegration[whl,#,iOrder]&@pos]]/;
With[{theRes=theFunc[0,0,0,1]},Print["iPtsRE:theRes=",theRes];
NumberQ[theRes]]




makeInterpFuncRE[theFunc_Function,pos_Integer,iOrder_Integer,iPts_Integer]:=
makeInterpFuncRE[theFunc,pos,iOrder,iPts,
{qLow,qHigh},
{ruLow,ruHigh}]





doScalarIntegration[whlList:{{{_?NumberQ..},_}..},pos_Integer,iOrder_Integer]:=
With[{prtList={#[[1]],(#[[2]]/.thePos->pos)}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]

oldmyExpect[aFunc_,aVar_]:=
NIntegrate @@ ({aFunc,{aVar,-4*sigma$u,4*sigma$u},
AccuracyGoal -> 2, Compiled -> Automatic,
  PrecisionGoal -> 2, WorkingPrecision -> 2}/.lucaSubs)
(*
NExpectation[Sow[Hold[With[{hoop=qrSubbed&[tryEps]},(*Print["cur",hoop];*)hoop[[theRowNow]]]]],tryEps \[Distributed] NormalDistribution[0,sigma$u]/.lucaSubs,
AccuracyGoal -> 2, Compiled -> Automatic, 
Method -> {"NIntegrate",{MinRecursion->1,MaxRecursion->2,AccuracyGoal->2}},
  PrecisionGoal -> 2, WorkingPrecision -> 2]
*)

myExpect[aFunc:fpForInitStateFunc[qVal_?NumberQ,ruVal_?NumberQ,epsVal_,
zFuncs_List,pos_Integer]*_,aVar_]:=
NIntegrate @@ ({aFunc,{aVar,-4*sigma$u,4*sigma$u},
AccuracyGoal -> 2, Compiled -> Automatic,
  PrecisionGoal -> 2, WorkingPrecision -> 2}/.lucaSubs)



doScalarInterp[whlList:{{{_?NumberQ..},{_?NumberQ..}}..},pos_Integer,iOrder_Integer]:=
With[{prtList={#[[1]],#[[2,pos]]}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]

preCalcInterp[iOrder_Integer,iPts_Integer,
{qLow_?NumberQ,qHigh_?NumberQ},
{ruLow_?NumberQ,ruHigh_?NumberQ}]:=
With[{theGrid=gridPts[iPts,
{qLow,qHigh},
{ruLow,ruHigh}],
fns=Table[Unique["fnVal"],{(iPts+1)^3}]},
With[{evals=Transpose[{theGrid,fns}]},
{fns,evals,Interpolation[evals,InterpolationOrder->iOrder]}]]


gridPts[iPts_Integer,
qRng:{qLow_?NumberQ,qHigh_?NumberQ},
rRng:{ruLow_?NumberQ,ruHigh_?NumberQ}]:=
With[{
qPts=oneDimGridPts[iPts,qRng],
rPts=oneDimGridPts[iPts,rRng]},
Flatten[Outer[List,qPts,rPts],1]]



oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]




iterPF[iOrder_Integer,numPts_Integer,zFuncsNow_List]:=With[
{fpSolnFunc=Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,zFuncsNow]]},
makeInterpFuncPF[fpSolnFunc,iOrder,numPts,{-.4,.4},{-.08,.08}]]/;
And[iOrder>=0,numPts>=iOrder]

iterRE[iOrder_Integer,numPts_Integer,zFuncsNow_List]:=With[
{fpSolnFunc=Function[{xx,yy,zz,pos},
fpForInitStateFunc[xx,yy,zz,zFuncsNow,pos]]},
makeInterpFuncRE[fpSolnFunc,iOrder,numPts,{-.4,.4},{-.08,.08}]]/;
And[iOrder>=0,numPts>=iOrder]


simPFPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=
With[{nn=Length[zFuncs]-1},
With[{firPt=getNextPt[qtm1Arg,rutm1Arg,epsArg,zFuncs[[-1,-1]]]},
With[{theVals=
NestList[{getNextPt[#[[1,1,1]],#[[1,3,1]],0,#1[[2,-1,-1]]],
Drop[#[[2]],-1]}&,{firPt,Drop[zFuncs,-1]},nn-1]},
Flatten[First/@theVals,1]]]]


simPFPath[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=
With[{firPt=getNextPt[qtm1Arg,rutm1Arg,epsArg,zFuncs[[-1,-1]]]},
With[{theVals=
NestList[{getNextPt[#[[1,1,1]],#[[1,3,1]],0,#1[[2,-1,-1]]],
Drop[#[[2]],-1]}&,{firPt,Drop[zFuncs,-1]},nn-1]},
First/@theVals]]




simPFPath[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]:=
With[{firPt=getNextPt[qtm1Arg,rutm1Arg,epsArg,noCnstrnGuessFuncs]},
With[{theVals=
NestList[getNextPt[#[[1,1]],#[[3,1]],0,noCnstrnGuessFuncs]&,firPt,nn-1]},
Flatten[theVals,1]]]


getNextPt[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=
aPath[qtm1Arg,rutm1Arg,epsArg,zFuncs][[Range[3]+3]]
(*

aPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=  
With[{thePathVals=
Drop[Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,zFuncs]][qtm1Arg,rutm1Arg,epsArg],2]},
With[{pathLen=Length[thePathVals]},
With[{tp=genPath[pathLen,1]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zLeft=(Drop[theZs,-0])},
With[{zAssn=Thread[zLeft->thePathVals]},
(*
Print["tp,theZs,zLeft,thePathVals,zAssn",{tp,theZs,zLeft,thePathVals,zAssn}];*)
tp/.zAssn
]]]]]
*)

aPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List,pad_Integer:0]:=  
With[{thePathVals=
Drop[Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,zFuncs]][qtm1Arg,rutm1Arg,epsArg],2]},
With[{pathLen=Length[thePathVals]},
With[{tp=genPath[pathLen,1+pad]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zLeft=(Drop[theZs,-0])},
With[{zAssn=Thread[zLeft->thePathVals]},
(*
Print["tp,theZs,zLeft,thePathVals,zAssn",{tp,theZs,zLeft,thePathVals,zAssn}];*)
tp/.zAssn
]]]]]


hmatApp[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,zFuncs_List]:=
With[{tp=aPath[qtm1,rutm1,eps,zFuncs]},
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar/.lucaSubs),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=hmat.tp[[Range[9]+3*#]]&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]]


nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ,fmat_?MatrixQ]:=
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[3]-fmat] . phimat . psic


genPath[numNonZeroZs_Integer,padZeroZs_Integer]:=
With[{startPath=genPath[numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,psieps,fmat])//numIt)&,startPath[[{-3,-2,-1}]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]





genPath[numNonZeroZs_Integer]:=
With[{xtm1={{qtm1},{rtm1},{rutm1}},
rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,1,0] &/@Range[0,numNonZeroZs-1])//numIt]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,psieps,fmat]+rawFParts[[1]])//numIt},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,psieps,fmat]+#2//numIt)&,
bgn,Drop[rawFParts,1]]]]]





doFEpsPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer]:=
doFEpsPart[phimat,fmat,psiz,horizon,numCon,0]


doFEpsPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer]:=
With[{zMats=doGenEpsVars[horizon,numCon,offset]},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer]:=
doFPart[phimat,fmat,psiz,horizon,numCon,0]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer]:=
With[{zMats=genZVars[horizon,numCon,offset]},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]


numericLinearizeSystemForOBC[eqns_List]:=
Module[{noCnstr=eqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx},zf,hr,
bmat,phimat,fmat},Print[noCnstr];
With[{hmat=equationsToMatrix[noCnstr]},Print[hmat];
{ig,ig,ig,ig,qmat,ig,ig,ig}=numericAMA[hmat,1,1];Print[zf,hf];
Print["need to generalize to actually compute qmat"];
{hmat,qmat,{bmat,phimat,fmat}=numericComputeBPhiF[hmat,qmat]}
]]

Print["defining luca model equations along with hmat, qmat, bmat, phimat and fmat"]



lucaSubs = {betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
   rUnderBar -> 2/100, qLow -> -.5, qHigh -> .5, 
   ruLow -> -4*sigma$u/(1 - rho$ru), 
   ruHigh ->  4*sigma$u/(1 - rho$ru), integOrder -> {20}, 
   sigma$u -> 0.01, theMean -> {0}, rho$ru -> 0.5, adj -> 1,rstar->0*3/100};

   (*modParams = {betap, phip, rhop, rho$ru, sigmap} //. lucaSubs // N;*)
   modParams = {rUnderBar} //. lucaSubs // N;
Protect[lucaSubs]


lucaEqns = {qq[t] - (betap*(1 - rhop)*qq[t + 1] + rhop*qq[t - 1] - 
      sigmap*rr[t] + ru[t]),
   (ru[t]-rstar) - rho$ru*(ru[t - 1]-rstar) - adj*eps[uu][t],
   rr[t] - eqvdIf[phip*qq[t] >= rUnderBar, phip*qq[t], rUnderBar]};
Protect[lucaEqns]





{hmat,qmat,{bmat,phimat,fmat}}=numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)];
psic={{0},{0},{(1-rho$ru)*rstar}};
psiz={{0},{0},{1}};
psieps={{0},{1},{0}};
Protect[hmat,qmat,bmat,phimat,fmat,psiz,psieps,psic];

aPath01=genPath[1]
try01={
(aPath01[[5,1]]>=(rUnderBar/.lucaSubs)&&zzz$0$1[t]==0)||
(aPath01[[5,1]]==(rUnderBar/.lucaSubs)&&zzz$0$1[t]>=0)
}

slv01=(Solve[try01,{zzz$0$1[t]},Reals])//FullSimplify//Chop
zzz$0$1Func= Function @@ {{qtm1,rutm1,eps},Piecewise[List @@@ (Last/@Flatten[slv01])]}




noCnstrnGuessFuncs={
Function[{xx,yy},noCnstrnGuess[xx,yy][[1]]],
Function[{xx,yy},noCnstrnGuess[xx,yy][[2]]],
Function[{xx,yy},0]}



z01ExactFunc=Function @@ {{qtm1,rutm1,eps},
With[{eval=zzz$0$1Func[qtm1,rutm1,eps]},
Append[Flatten[(genPath[1][[{4,6}]])],zzz$0$1[t]]/.
zzz$0$1[t]->eval]}

z01ExactInitPF={
Function[{xx,yy},z01ExactFunc[xx,yy,0][[1]]],
Function[{xx,yy},z01ExactFunc[xx,yy,0][[2]]],
Function[{xx,yy},z01ExactFunc[xx,yy,0][[3]]]}

Print["computing exact symbolic expression for z01 expectation"]
theExp=(Expectation[z01ExactFunc[qtm1,rutm1,eps],eps \[Distributed] NormalDistribution[0,sigma$u/.lucaSubs]])//FullSimplify

z01ExactExpFunc=Function @@ 
{{qtm1,rutm1},theExp//FullSimplify}

z01ExactInitRE={
Function[{xx,yy},z01ExactExpFunc[xx,yy][[1]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[2]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[3]]]}




(*always used with epsVal=0*)
noCnstrnGuess= With[{linPFSys=
Flatten[bmat . {{qVal},{0},{ruVal}}+phimat . (psieps *0+psic)]},
{Function @@ {{qVal,ruVal},linPFSys[[1]]},Function @@ {{qVal,ruVal},linPFSys[[2]]}}]



infNorm[func_]:=
NMaximize[
{Norm[func[qq,ru,ep],Infinity],
(((qLow<=qq<=qHigh)//.lucaSubs)//N)&&
(((ruLow<=ru<=ruHigh)//.lucaSubs)//N)&&
(((-2*sigma$u<=ep<=2*sigma$u)//.lucaSubs)//N)},
{qq,ru,ep},StepMonitor:>Print["step",{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}],EvaluationMonitor:>Print[{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}],Method->{"RandomSearch","SearchPoints"->50}]
Print["interp defs"]



pNorm[func_,pval_]:=
NMaximize[
{Norm[func[qq,ru,ep],p],
(((qLow<=qq<=qHigh)//.lucaSubs)//N)&&
(((ruLow<=ru<=ruHigh)//.lucaSubs)//N)&&
(((-2*sigma$u<=ep<=2*sigma$u)//.lucaSubs)//N)},
{qq,ru,ep}(*,StepMonitor:>Print["step",{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}],EvaluationMonitor:>Print[{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}]*),Method->{"RandomSearch","SearchPoints"->50}]
Print["interp defs"]



End[]
EndPackage[]
Print["done reading occBindRecur.m"]



(*




{cc,rs}=genCompSlackSysFunc[1]

{zVars,rhsEqns,initGuess,initStateSubbed,ts,fpnow}=
fpForInitStateFunc[-.4,.1,0]

{cc,rs}=genCompSlackSysFunc[2]

{zVars,rhsEqns,initGuess,initStateSubbed,ts,fpnow}=


numPts=5
Timing[doNow01= Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,z01ExactInitPF]]]
Timing[nxt01=makeInterpFuncPF[doNow01,1,numPts,{-.4,.4},{-.1,.1}]]

notnxt01=nxt01[[{1,2,4,4}]]
notdoNow02= Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,notnxt01]]
Timing[notnxt02=makeInterpFuncPF[notdoNow02,1,numPts,{-.4,.4},{-.1,.1}]]                                     


doNow02= Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,nxt01]]
Timing[nxt02=makeInterpFuncPF[doNow02,1,numPts,{-.4,.4},{-.1,.1}]]                                     



doNow03= Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,nxt02]]
Timing[nxt03=makeInterpFuncPF[doNow03,1,numPts,{-.4,.4},{-.1,.1}]]                                     
notnxt02[[1]][0,0,0]



NestList[iterPF[1,3,#]&,z01ExactInitPF,3]
FixedPointList[iterPF[1,3,#]&,z01ExactInitPF,50,SameTest->(
With[{chk={#1[[1]][-.4,-.1],#2[[1]][-.4,-.1]}},Print["cmp q val",chk,chk[[1]]-chk[[2]]];chk[[1]]==chk[[2]]]&)]

NestList[iterRE[1,3,#]&,z01ExactInitRE,3]
FixedPointList[iterPF[1,3,#]&,z01ExactInitPF,50,SameTest->(
With[{chk={#1[[1]][-.4,-.1],#2[[1]][-.4,-.1]}},Print["cmp q val",chk,chk[[1]]-chk[[2]]];chk[[1]]==chk[[2]]]&)]


numPts=2
Timing[doNow01PF= Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,z01ExactInitPF]]]
Timing[nxt01PF=makeInterpFuncPF[doNow01PF,1,numPts,{-.4,.4},{-.1,.1}]]



numPts=2
Timing[doNow01RE= Function[{xx,yy,zz,qq},fpForInitStateFunc[xx,yy,zz,z01ExactInitRE,qq]]]
Timing[nxt01RE=makeInterpFuncRE[doNow01RE,1,numPts,{-.4,.4},{-.1,.1}]]

numPts=2

do03=NestList[iterPF[1,3,#]&,{},3]
do03[[3,-1]]
do03[[2]]
better={do03[[3,{1,2}]],do03[[3,{3}]]]





*)



