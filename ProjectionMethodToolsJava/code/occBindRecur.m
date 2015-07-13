PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
Print["reading occBindRecur.m"]
BeginPackage["occBindRecur`",{"ProtectedSymbols`","ProjectionInterface`","JLink`","AMAModel`","NumericAMA`","SymbolicAMA`","mathSmolyak`"}]

genFinalPF::usage="genFinalPF[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]"
genFinalRE::usage="genFinalRE[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]"



genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,numNonZeroZs_Integer,padZeroZs_Integer]"


iterateDR::usage="iterateDR[drFunc_Function,initVec:{initQ_?NumberQ,initRu_?NumberQ,initEps_?NumberQ},stdev_?NumberQ,numPers_Integer,reps_Integer:1]"
hmatAppGeneric::usage="hmatAppGeneric[tp_?MatrixQ]"


infNormFinal::usage="infNormFinal[finalFuncs_List]"
infNormDeltaFinal::usage="infNormDeltaFinal[finalFuncsA_List,finalFuncsB_List]"

genFinalDR::usage="genFinalDR[finalFunc_List]"
resetSystem::usage="resetSyster[]"



newGenFinalRE::usage="newGenFinalRE[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]"

assessFunc::usage="assessPF[interpFuncFinal_List]"

makeInterpFuncFinal::usage="makeInterpFuncFinal[theFunc_Function,pos_List,iOrder_Integer,iPts_Integer,{qLow_?NumberQ,qHigh_?NumberQ},{ruLow_?NumberQ,ruHigh_?NumberQ},{epsLow_?NumberQ,epsHigh_?NumberQ}]"

smallestRVal::usage="smallestRVal[finalFuncs_List]"
simPFPath::usage="simPFPath[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"
simPFPathNoCnstrn::usage="simPFPathNoCnstrn[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]"
noCnstrnGuess::usage="no constraints guess lin system computation for q and ru"
aPathNoCnstrn::usage="aPathNoCnstrn[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ]"  

simPathFinal::usage="simPathFinal[nn_Integer,qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,finalFuncs_List]"
getNextPt::usage="getNextPt[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"
iterPF::usage="iterPF[iorder,numpts,zFuncsNow_List]"
iterRE::usage="iterRE[iorder,numpts,zFuncsNow_List,stdev]"
newIterRE::usage="newIterRE[iorder,numpts,zFuncsNow_List,stdev]"
aPathFinal::usage="aPathFinal[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,finalFuncs_List,pad_Integer:0]"
aPath::usage="aPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]"  
hmatApp::usage="hmatApp[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,zFuncs_List]"
hmatAppFinal::usage="hmatAppFinal[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,finalFuncs_List]"
infNorm::usage="infNorm[func_]"
(*assessLatestPF::usage="assessLatestPF[zFuncs_List]"*)
z01ExactInitRE::usage="exact ratex for one period"
z01ExactInitPF::usage="exact perfect foresight for one period"




genCompSlackSysFunc::usage="genCompSlack[pathLen_Integer,zFuncs:{_Function...}]"
fpForInitStateFunc::usage="fpForInitVecFunc[{compCon,stateSel,xtm1},{qVal,ruVal,epsVal},theCompSlackSysFunc_,zFuncs:{_Function...}]"


makeInterpFuncPF::usage="makeInterpFunc";
makeInterpFuncRE::usage="makeInterpFunc";
numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
symbolicLinearizeSystemForOBC::usage="symbolicLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ,fmat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"
forIOrdNPtsPF::usage="forIOrdNPtsPF[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
forIOrdNPtsRE::usage="forIOrdNPtsRE[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
doChkLoad::usage="doChkLoad[]->{numProcs,loadavgs..,freemem}"



(*assigned Private`ly below*)
hmat::usage="simple model matrix"
qmat::usage="simple model matrix"
bmat::usage="simple model matrix"
phimat::usage="simple model matrix"
fmat::usage="simple model matrix"
psiz::usage="simple model matrix"
psic::usage="simple model matrix"
psieps::usage="simple model matrix"
hmatSymb::usage="simple model matrix"
qmatSymb::usage="simple model matrix"
bmatSymb::usage="simple model matrix"
phimatSymb::usage="simple model matrix"
fmatSymb::usage="simple model matrix"
zfSymb::usage="simple model matrix"
hfSymb::usage="simple model matrix"
amatSymb::usage="simple model matrix"
evlsSymb::usage="simple model matrix"
evcsSymb::usage="simple model matrix"











Begin["Private`"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]

Print["occBindRecur: Turning off extrapolation warning messages"]
Off[InterpolatingFunction::dmval];

Print["need to split eps from other state vars"]
fpForInitStateFunc[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
xtm1Vals:{_?NumberQ..},
zFuncs_List,(pos_List)|(pos_Integer)]:=
With[{beenDone=fpForInitStateFunc[modSpecific,xtm1Vals,zFuncs]},
beenDone[[pos]]]





Print["fpForInitStateFunc still model specific"]
fpForInitStateFunc[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
xtm1Val:{_?NumberQ..,epsVal_?NumberQ},
zFuncs_List]:=
Module[{},
fpForInitStateFunc[modSpecific,
xtm1Val,zFuncs]=(*Print["disabled memoizing"];*)
With[{zValArgs=xtm1Val[[stateSel]],
zArgs=Table[Unique["xNow"],{Length[stateSel]}]},
With[{lhRule=(First/@(xtm1[[stateSel]])),
initGuess=If[Length[zFuncs]==0,
Through[noZFuncsGuess@@#&[zValArgs]],
Through[(zFuncs[[Range[Length[stateSel]]]]@@#&)[zValArgs]]],
pathLen=If[zFuncs==={},1,Length[zFuncs]-1]},
With[{valSubs=Append[Thread[lhRule->(xtm1Val[[Range[Length[lhRule]]]])],
eps->epsVal],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{andinittry=makeInitStateTryEqnsSubbed[modSpecific,zArgs,valSubs,pathLen]},
With[{zLeft=(Drop[theZs,-1])},
With[{theSys=makeSysFunction[pathLen,zFuncs,zLeft,andinittry,zArgs]},
With[{fpTarget=Join[zArgs,theZs]},(*Print["fpForInitStateFunc:",{fpTarget,theSys,initGuess}//InputForm];*)
getFixedPoint[fpTarget,theSys,initGuess]
]]]]]]]]/;
Or[zFuncs==={},
NumberQ[Plus @@ (Through[(zFuncs[[-1]])[.1,.1]])]]

mySameQ[xx_,yy_]:=And[Length[xx]===Length[yy],Norm[xx-yy]<=10^(-10)]


makeInitStateTryEqnsSubbed[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},zArgs_List,
valSubs_List,pathLen_Integer]:=
With[{csrhs=genCompSlackSysFunc[modSpecific,bmat,phimat,fmat,psieps,
psic,psiz,pathLen]/.valSubs},
With[{initStateSubbed=And @@ (csrhs[[1]]),
tryEqnsSubbed=And @@Thread[zArgs==(csrhs[[2]])]},
And[initStateSubbed,tryEqnsSubbed]]]

makeSysFunction[pathLen_Integer,
zFuncs_List,zLeft_List,initStateSubbedtryEqnsSubbed_,zArgs_List]:=
With[{xTryVars=Table[Unique["xTry"],{Length[zArgs]}]},
With[{theZFuncsApps=If[pathLen===1,{},
Through[(Drop[zFuncs,Length[zArgs]])@@ #&[xTryVars]]]},
With[{theZEqns=And @@ (Thread[zLeft==theZFuncsApps])},
With[{theGuts=And[initStateSubbedtryEqnsSubbed,theZEqns]},
With[{theFunc=Function @@{xTryVars,theGuts}},(*
Print["huh=",
{zLeft,initStateSubbedtryEqnsSubbed,theZFuncsApps,theZEqns,theGuts,theFunc[.1,.1]}];*)
theFunc]]]]]



getFixedPoint[fpTarget_List,theSys_Function,initGuess_List]:=
FixedPoint[fpTarget/.With[{soln=
Flatten[NSolve[(theSys @@ #),fpTarget]]},(*Print["soln=",soln,fpTarget];*)
If[Not[MatchQ[soln,{(_->_)..}]],Throw[{"NSolve Failed in >fpForInitState for",{theSys//InputForm,fpTarget,Stack[]}}],soln]]&,initGuess,SameTest->mySameQ]


 
forIOrdNPtsPF[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},start_List,ignore,maxLen_Integer]:=
NestList[Identity[iterPF[modSpecific,iOrd,gSpec[[{1,2}]],#]]&,start,maxLen];

forIOrdNPtsRE[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},start_List,stdev_?NumberQ,maxLen_Integer]:=
NestList[Identity[iterRE[modSpecific,iOrd,gSpec,#,stdev]]&,start,maxLen];

doChkLoad[]:=
If[$OperatingSystem=="Windows",{0,0,0,0,0},
Module[{},
Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"];
Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"];
Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"];
{numProcs,loadAvg,freeMem}//ToString//ToExpression]]





makeInterpFuncFinal[theFunc_Function,xtm1_?MatrixQ,
pos_List,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}
]:=Module[{thePts=
gridPts[gSpec]},
With[{whl={#,theFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{theRes=theFunc[.1,.1,.1]},Print["iPtsFinal:theRes=",theRes//InputForm];
NumberQ[Plus @@ theRes[[pos]]]]

makeInterpFuncPF[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
theFunc_Function,pos_List,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}
]:=Module[{thePts=gridPts[gSpec],
pfFunc=Function[{qq,ru},theFunc[qq,ru,0]]},
With[{whl={#,pfFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{theRes=theFunc[.1,.1,.1]},Print["iPtsPF:theRes=",theRes//InputForm];
NumberQ[Plus @@ theRes[[pos]]]]

makeInterpFuncPF[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
theFunc_Function,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}]:=
With[{pos=Range[Length[theFunc[.1,.1,.1]]]},Print["make pos=",{theFunc[.1,.1,.1],pos}];
makeInterpFuncPF[modSpecific,theFunc,pos,iOrder,gSpec]]










doScalarIntegration[whlList:{{{_?NumberQ..},_}..},pos_Integer,iOrder_Integer]:=
Module[{},(*Print["doScalarIntegration:",whlList//InputForm];*)
With[{prtList={#[[1]],(#[[2]]/.thePos->pos)}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]]




doScalarInterp[whlList:{{{_?NumberQ..},{_?NumberQ..}}..},pos_Integer,iOrder_Integer]:=
With[{prtList={#[[1]],#[[2,pos]]}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]





gridPts[rngs:{{_?NumberQ,_?NumberQ,_?NumberQ}..}]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
With[{theOuter=Outer[List,Sequence@@#]&[oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]





oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]



ageOneZFunc[qFunc_,ruFunc_,zFunc_]:=
Function[{qq,rr},zFunc[qFunc[qq,rr],ruFunc[qq,rr]]]

ageZFuncs[{}]:={}

ageZFuncs[zFuncs_List]:=
With[{qFunc=zFuncs[[1]],ruFunc=zFuncs[[2]]},
ageOneZFunc[qFunc,ruFunc,#]&/@Drop[zFuncs,2]]


simPFPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List,
nn_Integer]:=
With[{firPt=getNextPt[qtm1Arg,rutm1Arg,epsArg,zFuncs[[-1]]]},
With[{theVals=
NestList[{getNextPt[#[[1,1,1]],#[[1,3,1]],0,#1[[2,-1]]],
Drop[#[[2]],-1]}&,{firPt,Drop[zFuncs,-1]},nn-1]},
Join[{{qtm1Arg},{ignored},{rutm1Arg}},
Flatten[First/@theVals,1]]]]/;nn<Length[zFuncs]


simPFPath[
qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List,
nn_Integer]:=
With[{firstPart=simPFPath[qtm1Arg,rutm1Arg,epsArg,zFuncs]},
With[{secPart=simPFPathNoCnstrn[nn-Length[zFuncs],
firstPart[[-3,1]],firstPart[[-2,1]],0]},
Join[firstPart,Drop[secPart,3]]]]/;nn>Length[zFuncs]

simPFPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List,
nn_Integer]:=
simPFPath[qtm1Arg,rutm1Arg,epsArg,zFuncs]/;nn==Length[zFuncs]


simPFPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=
With[{nn=Length[zFuncs]},
With[{firPt=getNextPt[qtm1Arg,rutm1Arg,epsArg,zFuncs[[-1]]]},
With[{theVals=
NestList[{getNextPt[#[[1,1,1]],#[[1,3,1]],0,#1[[-1]]],
Drop[#[[2]],-1]}&,{firPt,Drop[zFuncs,-1]},nn-1]},
Join[{{qtm1Arg},{ignored},{rutm1Arg}},
Flatten[First/@theVals,1]]]]]



simPathFinal[
qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,finalFuncs_List,
nn_Integer:0]:=
aPathFinal[qtm1Arg,rutm1Arg,epsArg,finalFuncs,nn]/;nn>=0




simPFPathNoCnstrn[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,
nn_Integer]:=
aPathNoCnstrn[qtm1Arg,rutm1Arg,epsArg,nn]/;nn>0



genCompSlackSysFunc[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,
pathLen_Integer]:=
With[{aPath=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,
Length[compCon],pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{compConVal=Through[compCon[aPath,theZs]],
rhsEqns=First/@(Drop[aPath,Length[xtm1]][[stateSel]])},
{compConVal,rhsEqns}]]/;
And[pathLen>0]




genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,
numNonZeroZs_Integer,padZeroZs_Integer]:=
With[{startPath=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,numCon,numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,fmat,psieps,psic]))&,startPath[[{-3,-2,-1}]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]


genPath[xtm1_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,
numNonZeroZs_Integer]:=
With[{rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,numCon,0] &/@Range[0,numNonZeroZs-1])]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,fmat,psieps,psic]+rawFParts[[1]])},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,fmat,psieps,psic]+#2)&,bgn,Drop[rawFParts,1]]]]]




getNextPt[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List]:=
aPath[qtm1Arg,rutm1Arg,epsArg,zFuncs][[Range[3]+3]]

aPath[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
{qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ},zFuncs_List,pad_Integer:0]:=  
With[{thePathVals=
Drop[Function[{xx,yy,zz},
fpForInitStateFunc[modSpecific,{xx,yy,zz},zFuncs]][qtm1Arg,rutm1Arg,epsArg],2]},
With[{pathLen=Length[thePathVals]},
With[{tp=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,pathLen,1+pad]/.{xtm1[[1,1]]->qtm1Arg,xtm1[[3,1]]->rutm1Arg,eps->epsArg},
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zLeft=(Drop[theZs,-0])},
With[{zAssn=Thread[zLeft->thePathVals]},
(*
Print["tp,theZs,zLeft,thePathVals,zAssn",{tp,theZs,zLeft,thePathVals,zAssn}];*)
tp/.zAssn
]]]]]

aPathFinal[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,finalFuncs_List,pad_Integer:0]:=  
With[{thePathVals=
Drop[Through[finalFuncs[qtm1Arg,rutm1Arg,epsArg]],2]},
With[{pathLen=Length[thePathVals]},
With[{tp=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,pathLen,1+pad]/.{xtm1[[1,1]]->qtm1Arg,xtm1[[3,1]]->rutm1Arg,eps->epsArg},
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zLeft=(Drop[theZs,-0])},
With[{zAssn=Thread[zLeft->thePathVals]},
(*
Print["tp,theZs,zLeft,thePathVals,zAssn",{tp,theZs,zLeft,thePathVals,zAssn}];*)
tp/.zAssn
]]]]]



nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psimat_?MatrixQ,psic_?MatrixQ]:=
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic





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
bmat,phimat,fmat},(*Print[noCnstr];*)
With[{hmat=equationsToMatrix[noCnstr]},(*Print[hmat];*)
{ig,ig,ig,ig,qmat,ig,ig,ig}=numericAMA[hmat,1,1];(*Print[zf,hf];*)
Print["need to generalize to actually compute qmat"];
{hmat,qmat,{bmat,phimat,fmat}=numericComputeBPhiF[hmat,qmat]}
]]
symbolicLinearizeSystemForOBC[eqns_List]:=
Module[{noCnstr=eqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx},zf,hr,
bmat,phimat,fmat},(*Print[noCnstr];*)
With[{hmat=equationsToMatrix[noCnstr]},Print["symbolicLinearize:",hmat];
]]


infNormFinal[finalFuncs_List]:=
With[{finChk=
Function[{qq,ru,eps}, Max[Abs[hmatAppFinal[qq,ru,eps,finalFuncs]]]]},
infNorm[finChk]]

infNormDeltaFinal[finalFuncsA_List,finalFuncsB_List]:=
With[{finDel=
Function[{qq,ru,eps},With[{
aEval=aPathFinal[qq,ru,eps,finalFuncsA],
bEval=aPathFinal[qq,ru,eps,finalFuncsB]},
Flatten[
(aEval[[3+Range[3]]])-
(bEval[[3+Range[3]]])]]]},
finDel]



assessFunc[interpFuncFinal_List]:=
{smallestRVal[interpFuncFinal],infNormFinal[interpFuncFinal]}








genFinalDR[finFuncs_List]:=Function[{qq,ru,eps},
Flatten[aPathFinal[qq,ru,eps,finFuncs][[3+Range[3]]]]]



genFinalPF[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
initFuncs_List,iters_Integer:1]:=
genFinalWorker[modSpecific(*modSpecific*),
forIOrdNPtsPF,iOrd,gSpec,
initFuncs,ignore,iters]

genFinalRE[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
initFuncs_List,stdev_?NumberQ,iters_Integer:1]:=
genFinalWorker[modSpecific(*modSpecific*),
forIOrdNPtsRE,iOrd,gSpec,
initFuncs,stdev,iters]
(*put std dev = 0 in ratex*)

genFinalWorker[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
forIOrdNPtsFunc_,
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},initFuncs_List,
stdev:(_?NumberQ|ignore),iters_Integer:1]:=
With[{zFuncs=forIOrdNPtsFunc[modSpecific,
iOrd,gSpec,initFuncs,stdev,iters]},
With[{preInterpFunc=
Function[{qq,ru,eps},fpForInitStateFunc[modSpecific,
{qq,ru,eps},zFuncs[[-1]]]]},
With[{numVals=Length[preInterpFunc[.1,.1,.1]]},
With[{interpFuncFinal=
makeInterpFuncFinal[preInterpFunc,xtm1,Range[numVals],
iOrd,gSpec]},
{{iOrd,gSpec},{},zFuncs,interpFuncFinal}]]]]





iterateDR[drFunc_Function,
initVec:{initQ_?NumberQ,initRu_?NumberQ,initEps_?NumberQ},
stdev_?NumberQ,numPers_Integer,reps_Integer:1]:=
With[{firVal=drFunc @@ initVec},
With[{allReps=
Table[
NestList[drFunc @@ {#[[1]],#[[3]],
If[stdev==0,0,RandomVariate[NormalDistribution[0,stdev]]]}&,firVal,numPers-1],{reps}]},
With[{theMean=prepMeansForHApp[Mean[allReps],initVec]},
If[reps==1,theMean,
{theMean,prepStdDevsForHApp[StandardDeviation[allReps]]}]]]]/;
And[reps>0,numPers>0]

chkIterateDR[drFunc_Function,
initVec:{initQ_?NumberQ,initRu_?NumberQ,initEps_?NumberQ},
stdev_?NumberQ,numPers_Integer,reps_Integer:1]:=
With[{firVal=drFunc @@ initVec},
With[{allReps=
Table[
NestList[drFunc @@ {#[[1]],#[[3]],0}&,firVal,numPers-1],{reps}]},
allReps]]/;
And[reps>0,numPers>0]


prepMeansForHApp[theMeans_List,initVec_List]:=
Join[Transpose[{initVec}],Transpose[{Flatten[theMeans,1]}]]

prepStdDevsForHApp[theStdDevs_List]:=
Join[Transpose[{{0,0,0}}],Transpose[{Flatten[theStdDevs,1]}]]

resetSystem:=Module[{},
Clear[fpForInitStateFunc];
Get["occBindRecur`"]]







makeInterpFuncRE[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},theFunc_Function,pos_Integer,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},stdev_?NumberQ]:=
Module[{thePts=gridPts[noShocksGSpec[gSpec]],
reFunc=Function @@ {{qq,ru},
With[{qrSubbed=theFunc[qq,ru,#,thePos]},
(*Print["about to use myExpect in func:",{compCon,stateSel,xtm1,Identity[Identity[With[{hoop=(qrSubbed&[tryEps])},hoop]]],tryEps,stdev,qrSubbed}];*)
myExpect[modSpecific,Identity[Identity[With[{hoop=(qrSubbed&[tryEps])},hoop]]],tryEps,stdev]]}},(*Print["done use myExpect"];*)
(*Print["reFunc=",reFunc//InputForm];*)
With[{whl={#,reFunc @@ #}& /@
thePts},(*Print["done making whl"];*)
doScalarIntegration[whl,#,iOrder]&@pos]]/;
With[{theRes=theFunc[.1,.1,.1,1]},Print["iPtsRE:theRes=",theRes];
NumberQ[theRes]]

noShocksGSpec[gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}]:=
Drop[gSpec,-1]

Print["try reusing modSpecific in definition"]
myExpect[
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
aFuncNow:fpForInitStateFunc[
{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
{qVal_?NumberQ,ruVal_?NumberQ,epsVal_},
zFuncs_List,pos_Integer],aVar_,stdev_?NumberQ]:=
Module[{},(*Print["myExpect:",{aFunc,aVar,aFuncNow,stdev}//InputForm];*)
If[stdev==0,(*Print["aFunc subbed:",{aFunc/.aVar->0,aFuncNow/.aVar->0}];*)aFuncNow/.aVar->0,
With[{theIntBody=({aFuncNow*PDF[NormalDistribution[0,stdev],tryEps],{aVar,-4*stdev,4*stdev}(*,
AccuracyGoal -> 2, Compiled -> Automatic,
  PrecisionGoal -> 2, WorkingPrecision -> 2*)})},(*Print["myExpect:intBody=",theIntBody//InputForm];*)
NIntegrate @@ theIntBody]]]



iterPF[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrder_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},zFuncsNow_List]:=With[
{fpSolnFunc=Function[{xx,yy,zz},fpForInitStateFunc[modSpecific,
{xx,yy,zz},zFuncsNow]]},
makeInterpFuncPF[modSpecific,fpSolnFunc,iOrder,gSpec]]/;
And[iOrder>=0,Min[First/@gSpec]>=iOrder]

iterRE[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_},
iOrder_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
zFuncsNow_List,stdev_?NumberQ]:=
With[{
agedZs=ageZFuncs[zFuncsNow],
fpSolnFunc=Function[{xx,yy,zz,pos},
fpForInitStateFunc[modSpecific,
{xx,yy,zz},zFuncsNow,pos]]},
With[
{forQ=makeInterpFuncRE[modSpecific,fpSolnFunc,1,iOrder,gSpec,stdev],
forRu=makeInterpFuncRE[modSpecific,fpSolnFunc,2,iOrder,gSpec,stdev],
forNewZ=makeInterpFuncRE[modSpecific,fpSolnFunc,-1,iOrder,gSpec,stdev]},
With[{newRes=Join[{forQ,forRu},agedZs,{forNewZ}]},newRes]]]/;
And[iOrder>=0,Min[First/@gSpec]>=iOrder]


aPathNoCnstrn[
qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,pad_Integer:1]:=  
With[{epsImpact=Flatten[phimat . psieps *epsArg+
Inverse[IdentityMatrix[3]-fmat] . phimat . psic ],
pathIterFunc=Function @@{{qq,rr,ru},Flatten[
((bmat . {{qq},{rr},{ru}})+  Inverse[IdentityMatrix[3]-fmat] . phimat . psic )]}},
With[{pathVals=NestList[pathIterFunc @@ #&,
(pathIterFunc @@ {qtm1Arg,rr,rutm1Arg})+epsImpact,pad-1]},
Join[{{qtm1Arg},{ignored},{rutm1Arg}},Transpose[{Flatten[pathVals]}]]]]/;pad>0


hmatApp[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,zFuncs_List]:=
With[{tp=aPath[qtm1,rutm1,eps,zFuncs]},
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]]


hmatAppFinal[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,finalFuncs_List]:=
With[{tp=aPathFinal[qtm1,rutm1,eps,finalFuncs]},
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]]

hmatAppGeneric[tp_?MatrixQ,eps_:0]:=
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]








End[]
EndPackage[]
Print["done reading occBindRecur.m"]




