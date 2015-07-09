PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
Print["reading occBindRecur.m"]
BeginPackage["occBindRecur`",{"ProtectedSymbols`","ProjectionInterface`","JLink`","AMAModel`","NumericAMA`","SymbolicAMA`","mathSmolyak`"}]

genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,numNonZeroZs_Integer,padZeroZs_Integer]"


iterateDR::usage="iterateDR[drFunc_Function,initVec:{initQ_?NumberQ,initRu_?NumberQ,initEps_?NumberQ},stdev_?NumberQ,numPers_Integer,reps_Integer:1]"
hmatAppGeneric::usage="hmatAppGeneric[tp_?MatrixQ]"


infNormFinal::usage="infNormFinal[finalFuncs_List]"
infNormDeltaFinal::usage="infNormDeltaFinal[finalFuncsA_List,finalFuncsB_List]"

genFinalDR::usage="genFinalDR[finalFunc_List]"
resetSystem::usage="resetSyster[]"



genFinalPF::usage="genFinalPF[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]"
genFinalRE::usage="genFinalRE[iOrd_Integer,nPts_Integer,initFuncs_List,iters_Integer:1]"
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
fpForInitStateFunc::usage="fpForInitVecFunc[compCon,stateSel,qVal,ruVal,epsVal,theCompSlackSysFunc_,zFuncs:{_Function...}]"


makeInterpFuncPF::usage="makeInterpFunc";
makeInterpFuncRE::usage="makeInterpFunc";
numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
symbolicLinearizeSystemForOBC::usage="symbolicLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ,fmat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"
forIOrdNPtsPF::usage="forIOrdNPtsPF[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
forIOrdNPtsRE::usage="forIOrdNPtsRE[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
newForIOrdNPtsRE::usage="newForIOrdNPtsRE[iOrd_Integer,nPts_Integer,start_List,maxLen_Integer]"
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


fpForInitStateFunc[compCon:{_Function...},stateSel_Function,
qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
zFuncs_List,(pos_List)|(pos_Integer)]:=
With[{beenDone=fpForInitStateFunc[compCon,stateSel,qVal,ruVal,epsVal,zFuncs]},
beenDone[[pos]]]


Print["fpForInitStateFunc still model specific"]

(*before refactor to split

fpForInitStateFunc[compCon:{_Function...},stateSel_Function,
qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
zFuncs_List]:=
Module[{},
fpForInitStateFunc[compCon,stateSel,
qVal,ruVal,epsVal,zFuncs]=(*Print["disabled memoizing"];*)
With[{pathLen=If[zFuncs==={},1,Length[zFuncs]-1],
valSubs={qtm1->qVal,rutm1->ruVal,eps->epsVal}},(*Print["valsubs",valSubs];*)
With[{csrhs=genCompSlackSysFunc[compCon,stateSel,
{{qtm1},{rtm1},{rutm1}},bmat,phimat,fmat,psieps,
psic,psiz,pathLen]/.valSubs,
initGuess=If[Length[zFuncs]==0,
Through[noCnstrnGuess[qVal,ruVal]][[{1,2}]],
{zFuncs[[1]][qVal,ruVal],zFuncs[[2]][qVal,ruVal]}],
aPath=genPath[{{qtm1},{rtm1},{rutm1}},
bmat,phimat,fmat,psieps,psic,psiz,pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{initStateSubbed=And @@ (csrhs[[1]]),
tryEqnsSubbed=And @@Thread[{qTry,rTry}==(csrhs[[2]])]},
With[{zLeft=(Drop[theZs,-1])},
With[{theSys=Function[{qTry,rTry},
With[{zFuncsApps=If[pathLen===1,{},Through[Drop[zFuncs,2][qTry,rTry]]]},
With[{zEqns=And @@ (Thread[zLeft==zFuncsApps])},
And[initStateSubbed,zEqns,tryEqnsSubbed]]]]},
With[{zVars=Union[Cases[initStateSubbed,xx_[t],Infinity]]},
With[{fpTarget=Join[{qTry,rTry},theZs]},
If[Drop[Union[fpTarget],2]=!=Union[theZs],Print["diff zs",Drop[Union[fpTarget],2],Union[theZs],zEqns]];(*Print["tosolve",{csrhs,theSys,fpTarget,initGuess,zFuncs}//InputForm];*)
FixedPoint[fpTarget/.With[{soln=
Flatten[NSolve[theSys @@ #,fpTarget]]},(*Print["soln=",soln,fpTarget];*)
If[Not[MatchQ[soln,{(_->_)..}]],Throw[{"NSolve Failed in >fpForInitState for",{theSys,fpTarget}}],soln]]&,initGuess,SameTest->mySameQ]]]]]]]]]/;
Or[zFuncs==={},
(*Print["make",{Through[(zFuncs[[-1]])[0,0]],(zFuncs)//InputForm}];*)
NumberQ[Plus @@ (Through[(zFuncs[[-1]])[0,0]])]]
*)

Print["fpForInitStateFunc still model specific"]
fpForInitStateFunc[compCon:{_Function...},stateSel_Function,
qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
zFuncs_List]:=
Module[{},
fpForInitStateFunc[compCon,stateSel,
qVal,ruVal,epsVal,zFuncs]=(*Print["disabled memoizing"];*)
With[{pathLen=If[zFuncs==={},1,Length[zFuncs]-1],
valSubs={qtm1->qVal,rutm1->ruVal,eps->epsVal}},(*Print["valsubs",valSubs];*)
With[{csrhs=genCompSlackSysFunc[compCon,stateSel,
{{qtm1},{rtm1},{rutm1}},bmat,phimat,fmat,psieps,
psic,psiz,pathLen]/.valSubs,
initGuess=If[Length[zFuncs]==0,
Through[noCnstrnGuess[qVal,ruVal]][[{1,2}]],
{zFuncs[[1]][qVal,ruVal],zFuncs[[2]][qVal,ruVal]}],
aPath=genPath[{{qtm1},{rtm1},{rutm1}},
bmat,phimat,fmat,psieps,psic,psiz,pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{initStateSubbed=And @@ (csrhs[[1]]),
tryEqnsSubbed=And @@Thread[{qTry,rTry}==(csrhs[[2]])]},
With[{zLeft=(Drop[theZs,-1])},
With[{theSys=
makeSysFunction[pathLen,
zFuncs,zLeft,initStateSubbed,tryEqnsSubbed]},
With[{fpTarget=Join[{qTry,rTry},theZs]},
getFixedPoint[fpTarget,theSys,initGuess]
]]]]]]]/;
Or[zFuncs==={},
(*Print["make",{Through[(zFuncs[[-1]])[0,0]],(zFuncs)//InputForm}];*)
NumberQ[Plus @@ (Through[(zFuncs[[-1]])[0,0]])]]

mySameQ[xx_,yy_]:=And[Length[xx]===Length[yy],Norm[xx-yy]<=10^(-10)]


makeSysFunction[pathLen_Integer,
zFuncs_List,zLeft_List,initStateSubbed_,tryEqnsSubbed_]:=
Function[{qTry,rTry},
With[{zFuncsApps=If[pathLen===1,{},Through[Drop[zFuncs,2][qTry,rTry]]]},
With[{zEqns=And @@ (Thread[zLeft==zFuncsApps])},
And[initStateSubbed,zEqns,tryEqnsSubbed]]]]



getFixedPoint[fpTarget_List,theSys_Function,initGuess_List]:=
FixedPoint[fpTarget/.With[{soln=
Flatten[NSolve[theSys @@ #,fpTarget]]},(*Print["soln=",soln,fpTarget];*)
If[Not[MatchQ[soln,{(_->_)..}]],Throw[{"NSolve Failed in >fpForInitState for",{theSys,fpTarget}}],soln]]&,initGuess,SameTest->mySameQ]


 
forIOrdNPtsPF[compCon:{_Function...},stateSel_Function,
iOrd_Integer,gSpec:{qPts_Integer,rPts_Integer,ePts_Integer},start_List,ignore,maxLen_Integer]:=
NestList[Identity[iterPF[compCon,stateSel,iOrd,gSpec[[{1,2}]],#]]&,start,maxLen];

forIOrdNPtsRE[compCon:{_Function...},stateSel_Function,
iOrd_Integer,gSpec:{qPts_Integer,rPts_Integer,ePts_Integer},start_List,stdev_?NumberQ,maxLen_Integer]:=
NestList[Identity[iterRE[compCon,stateSel,iOrd,gSpec,#,stdev]]&,start,maxLen];

doChkLoad[]:=
If[$OperatingSystem=="Windows",{0,0,0,0,0},
Module[{},
Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"];
Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"];
Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"];
{numProcs,loadAvg,freeMem}//ToString//ToExpression]]





makeInterpFuncFinal[theFunc_Function,pos_List,iOrder_Integer,
gSpec:{{_Integer,qLow_?NumberQ,qHigh_?NumberQ},
{_Integer,ruLow_?NumberQ,ruHigh_?NumberQ},
{_Integer,epsLow_?NumberQ,epsHigh_?NumberQ}}
]:=Module[{thePts=
gridPts[gSpec]},
With[{whl={#,theFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{theRes=theFunc[0,0,0]},Print["iPtsFinal:theRes=",theRes];
NumberQ[Plus @@ theRes[[pos]]]]

makeInterpFuncPF[compCon:{_Function...},stateSel_Function,
theFunc_Function,pos_List,iOrder_Integer,
gSpec:{{_Integer,qLow_?NumberQ,qHigh_?NumberQ},
{_Integer,ruLow_?NumberQ,ruHigh_?NumberQ}}
]:=Module[{thePts=gridPts[gSpec],
pfFunc=Function[{qq,ru},theFunc[qq,ru,0]]},
With[{whl={#,pfFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{theRes=theFunc[0,0,0]},Print["iPtsPF:theRes=",theRes];
NumberQ[Plus @@ theRes[[pos]]]]

makeInterpFuncPF[compCon:{_Function...},stateSel_Function,
theFunc_Function,iOrder_Integer,
gSpec:{{_Integer,qLow_?NumberQ,qHigh_?NumberQ},
{_Integer,ruLow_?NumberQ,ruHigh_?NumberQ}}]:=
With[{pos=Range[Length[theFunc[0,0,0]]]},
makeInterpFuncPF[compCon,stateSel,theFunc,pos,iOrder,gSpec]]










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



genCompSlackSysFunc[compCon:{_Function...},stateSel_Function,
xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,
pathLen_Integer]:=
With[{aPath=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,
Length[compCon],pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{compConVal=Through[compCon[aPath,theZs]],
rhsEqns=stateSel[aPath]},
{compConVal,rhsEqns}]]/;
And[pathLen>0]




genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,
numNonZeroZs_Integer,padZeroZs_Integer]:=
With[{startPath=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,numNonZeroZs]},
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

aPath[qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,zFuncs_List,pad_Integer:0]:=  
With[{thePathVals=
Drop[Function[{xx,yy,zz},fpForInitStateFunc[xx,yy,zz,zFuncs]][qtm1Arg,rutm1Arg,epsArg],2]},
With[{pathLen=Length[thePathVals]},
With[{tp=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,pathLen,1+pad]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},
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
With[{tp=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,pathLen,1+pad]/.{qtm1->qtm1Arg,rutm1->rutm1Arg,eps->epsArg},
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

End[]
EndPackage[]
Print["done reading occBindRecur.m"]



