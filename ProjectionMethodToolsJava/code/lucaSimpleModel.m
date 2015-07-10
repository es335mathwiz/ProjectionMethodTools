Print["reading lucaSimpleModel.m"]
BeginPackage["lucaSimpleModel`",{"occBindRecur`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","ProjectionInterface`"}]


compCon::usage="compCon[aPath_?MatrixQ]:=Function[{aPath,theZs}"
stateSel::usage="stateSel select state variables needed for iteration"
lucaEqns::usage="simple model equations"
lucaSubs::usage="typical simple model parameters"
latexSubs::usage="latex simplifying subs"
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
qLow::usage="parameter for model"
qHigh::usage="parameter for model"
integOrder::usage="parameter for model"
theMean::usage="parameter for model"
ruLow::usage="parameter for model"
ruHigh::usage="parameter for model"
ignored::usage="placeholder for ignored vec element"

Protect[qq,rr,ru,rhop,betap,sigmap,rho$ru,adj,uu,phip,rUnderBar,sigma$u,rstar,ignored,qLow,qHigh,ruLow,ruHigh,integOrder,theMean]


numIt::usage="numIt[xx_]  does lucaSubs and leaves t values untouched by N[]"

Begin["Private`"]






numIt[xx_]:=xx//.lucaSubs//myN//Expand//Chop 
Print["defining luca model equations along with hmat, qmat, bmat, phimat and fmat"]



lucaSubs = {betap -> 1/100(*99/100*), phip -> 1, rhop -> 1/2, sigmap -> 1, 
   rUnderBar -> 2/100, qLow -> -.5, qHigh -> .5, 
   ruLow -> -4*sigma$u/(1 - rho$ru), 
   ruHigh ->  4*sigma$u/(1 - rho$ru), integOrder -> {20}, 
   sigma$u -> 0.01, theMean -> {0}, rho$ru -> 0.5, adj -> 1,rstar->3/100};

   (*modParams = {betap, phip, rhop, rho$ru, sigmap} //. lucaSubs // N;*)
   modParams = {rUnderBar} //. lucaSubs // N;
Protect[lucaSubs]


lucaEqns = {qq[t] - (betap*(1 - rhop)*qq[t + 1] + rhop*qq[t - 1] - 
      sigmap*rr[t] + ru[t]),
   (ru[t]-rstar) - rho$ru*(ru[t - 1]-rstar) - adj*eps[uu][t],
   rr[t] - eqvdIf[phip*qq[t] >= rUnderBar, phip*qq[t], rUnderBar]};
Protect[lucaEqns]



{hmat,qmat,{bmat,phimat,fmat}}=(numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)]//myN);
psic=({{0},{(1-rho$ru)*rstar},{0}}//.lucaSubs)//myN;
psiz={{0},{0},{1}};
psieps={{0},{1},{0}};


hmatSymb=equationsToMatrix[
lucaEqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx}]//FullSimplify;
{zfSymb,hfSymb}=symbolicAR[hmatSymb];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{5}]]];
{bmatSymb,phimatSymb,fmatSymb}=symbolicComputeBPhiF[hmatSymb,qmatSymb]//FullSimplify;
Protect[hmat,qmat,bmat,phimat,fmat,psiz,psieps,psic,
hmatSymb,qmatSymb,bmatSymb,phimatSymb,fmatSymb,
zfSymb,hfSymb,amatSymb,evlsSymb,evcsSymb];




forSimp = Union[Cases[bmatSymb, Sqrt[___], Infinity]][[1]];
latexSubs = Join[N[lucaSubs],{(*forSimp -> \[Nu],*) 
betap -> Subscript[\[Beta], p], 
   phip -> Subscript[\[Phi], p], rhop -> Subscript[\[Rho], p], 
   rho$ru -> Subscript[\[Rho], ru], sigmap -> Subscript[\[Sigma], p],
xx_[yy:t+_.]->Subscript[xx,yy],zt0->Subscript[z,0],
uu$Shock->Subscript[\[Epsilon],t],
Subscript[discrep,t]->\[Delta],eqvdIf->\[CurlyTheta],
Subscript[ru,y:t+_.]->Subscript[Subscript[r,u],y]}];
Export["prettyHmatSymb.pdf", MatrixForm[hmatSymb //. latexSubs]];
Export["prettyBmatSymb.pdf", MatrixForm[bmatSymb //. latexSubs]];
Export["prettyPhimatSymb.pdf", MatrixForm[phimatSymb //. latexSubs]];
Export["prettyFmatSymb.pdf", MatrixForm[fmatSymb //. latexSubs]];
Export["prettyPsiEps.pdf", 
 MatrixForm[psieps]]; Export["prettyPsiZ.pdf", 
 MatrixForm[psiz]]; Export["prettyPsiEps.pdf", MatrixForm[psieps]];















(*

genPath[numNonZeroZs_Integer,padZeroZs_Integer]:=
With[{startPath=genPath[numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,psieps,fmat])//myN)&,startPath[[{-3,-2,-1}]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]


genPath[numNonZeroZs_Integer]:=
With[{xtm1={{qtm1},{rtm1},{rutm1}},
rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,1,0] &/@Range[0,numNonZeroZs-1])//myN]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,psieps,fmat]+rawFParts[[1]])//myN},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,psieps,fmat]+#2//myN)&,bgn,Drop[rawFParts,1]]]]]

*)
(*need to factor out model below*)


aPath01=genPath[{{qtm1},{rtm1},{rutm1}},bmat,phimat,fmat,psieps,1]//numIt
try01={
(aPath01[[5,1]]>=0.02&&zzz$0$1[t]==0)||
(aPath01[[5,1]]==0.02&&zzz$0$1[t]>=0)
}

If[Length[slv01]==0,
slv01=(Solve[try01,{zzz$0$1[t]},Reals])//FullSimplify//Chop;
zzz$0$1Func= Function @@ {{qtm1,rutm1,eps},Piecewise[List @@@ (Last/@Flatten[slv01])]}]



noCnstrnGuessFuncs={
Function[{xx,yy},noCnstrnGuess[xx,yy][[1]]],
Function[{xx,yy},noCnstrnGuess[xx,yy][[2]]],
Function[{xx,yy},0]}



z01ExactFunc=Function @@ {{qtm1,rutm1,eps},
With[{eval=zzz$0$1Func[qtm1,rutm1,eps]},
Append[Flatten[(genPath[{{qtm1},{rtm1},{rutm1}},bmat,phimat,fmat,psieps,1][[{4,6}]])],zzz$0$1[t]]/.
zzz$0$1[t]->N[eval]]}

z01ExactInitPF={
Function[{xx,yy},z01ExactFunc[xx,yy,0][[1]]],
Function[{xx,yy},z01ExactFunc[xx,yy,0][[2]]],
Function[{xx,yy},z01ExactFunc[xx,yy,0][[3]]]}

If[Length[theExp]==0,
Print["computing exact symbolic expression for z01 expectation"];
theExp=(Expectation[z01ExactFunc[qtm1,rutm1,eps],eps \[Distributed] NormalDistribution[0,sigma$u/.lucaSubs]])//FullSimplify;
]
z01ExactExpFunc=Function @@ 
{{qtm1,rutm1},theExp//FullSimplify}

z01ExactInitRE={
Function[{xx,yy},z01ExactExpFunc[xx,yy][[1]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[2]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[3]]]}




(*always used with epsVal=0*)
noCnstrnGuess= With[{linPFSys=
Flatten[bmat . {{qVal},{0},{ruVal}}+phimat . (psieps *0+psic)]//.lucaSubs},
{Function @@ {{qVal,ruVal},linPFSys[[1]]},Function @@ {{qVal,ruVal},linPFSys[[2]]}}]

Print["defining constraint and selector funcs"]
compCon={Function[{aPath,theZs},
(((aPath[[5,1]])>=(0.02(*rUnderBar//.lucaSubs*))&&theZs[[-1]]==0)||
((aPath[[5,1]])==(0.02(*rUnderBar//.lucaSubs*))&&theZs[[-1]]>=0))]}


stateSel=Function[aVec,{aVec[[1]],aVec[[3]]}]
stateSel={1,3};

(*only set delayed*)

infNorm[func_]:=
NMaximize[
{Norm[func[qq,ru,ep],Infinity],
(((qLow<=qq<=qHigh)//.lucaSubs)//N)&&
(((ruLow<=ru<=ruHigh)//.lucaSubs)//N)&&
(((-2*sigma$u<=ep<=2*sigma$u)//.lucaSubs)//N)},
{qq,ru,ep}(*,StepMonitor:>Print["step",{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}],EvaluationMonitor:>Print[{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}]*),Method->{"RandomSearch","SearchPoints"->50}]
Print["interp defs"]


smallestRFunc[finalFuncs_List]:=
With[{anEval=aPathFinal[0,0,0,finalFuncs]},
With[{theRpos=Range[5,Length[anEval]-3,3]},
With[{theRFunc=
Function[{xx,yy,zz},Min[Flatten[
aPathFinal[xx,yy,zz,finalFuncs][[theRpos]]]]]},
theRFunc]]]



smallestRVal[finalFuncs_List]:=
With[{smallFunc=smallestRFunc[finalFuncs]},
NMinimize[
{Hold[smallFunc[qq,ru,ep]],
(((qLow<=qq<=qHigh)//.lucaSubs)//N)&&
(((ruLow<=ru<=ruHigh)//.lucaSubs)//N)&&
(((-2*sigma$u<=ep<=2*sigma$u)//.lucaSubs)//N)},
{qq,ru,ep}(*,StepMonitor:>Print["step",{{qq,ru,ep},smallFunc[qq,ru,ep]}],EvaluationMonitor:>Print[{{qq,ru,ep},smallFunc[qq,ru,ep]}]*),"RandomSeed"->1,Method->{"RandomSearch","SearchPoints"->50}]]




pNorm[func_,pval_]:=
NMaximize[
{Norm[func[qq,ru,ep],p],
(((qLow<=qq<=qHigh)//.lucaSubs)//N)&&
(((ruLow<=ru<=ruHigh)//.lucaSubs)//N)&&
(((-2*sigma$u<=ep<=2*sigma$u)//.lucaSubs)//N)},
{qq,ru,ep}(*,StepMonitor:>Print["step",{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}],EvaluationMonitor:>Print[{{qq,ru,ep},func[qq,ru,ep],Abs[func[qq,ru,ep]]}]*),Method->{"RandomSearch","SearchPoints"->50}]
Print["interp defs"]

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



genFinalPF[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
iOrd_Integer,{qPts_Integer,rPts_Integer,ePts_Integer},
initFuncs_List,iters_Integer:1]:=
genFinalWorker[{compCon,stateSel,xtm1},
forIOrdNPtsPF,iOrd,{qPts,rPts,ePts},
initFuncs,ignore,iters]

genFinalRE[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
iOrd_Integer,{qPts_Integer,rPts_Integer,ePts_Integer},
initFuncs_List,stdev_?NumberQ,iters_Integer:1]:=
genFinalWorker[{compCon,stateSel,xtm1},
forIOrdNPtsRE,iOrd,{qPts,rPts,ePts},
initFuncs,stdev,iters]
(*put std dev = 0 in ratex*)

genFinalWorker[modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
forIOrdNPtsFunc_,
iOrd_Integer,gSpec:{qPts_Integer,rPts_Integer,ePts_Integer},initFuncs_List,
stdev:(_?NumberQ|ignore),iters_Integer:1]:=
With[{zFuncs=forIOrdNPtsFunc[{compCon,stateSel,xtm1},
iOrd,gSpec,initFuncs,stdev,iters]},
With[{preInterpFunc=
Function[{qq,ru,eps},fpForInitStateFunc[{compCon,stateSel,xtm1},
{qq,ru,eps},zFuncs[[-1]]]]},
With[{numVals=Length[preInterpFunc[0,0,0]]},
With[{interpFuncFinal=
makeInterpFuncFinal[preInterpFunc,xtm1,Range[numVals],
iOrd,{
{qPts,qLow//.lucaSubs//N,qHigh//.lucaSubs//N},
{rPts,ruLow//.lucaSubs//N,ruHigh//.lucaSubs//N},
{ePts,-2*sigma$u//.lucaSubs//N,2*sigma$u//.lucaSubs//N}}]},
{{iOrd,{qPts,rPts,ePts}},{},zFuncs,interpFuncFinal}]]]]





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
modSpecific:{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},theFunc_Function,pos_Integer,iOrder_Integer,
gSpec:{{_Integer,qLow_?NumberQ,qHigh_?NumberQ},
{_Integer,ruLow_?NumberQ,ruHigh_?NumberQ}},stdev_?NumberQ]:=
Module[{thePts=gridPts[gSpec],
reFunc=Function @@ {{qq,ru},
With[{qrSubbed=theFunc[qq,ru,#,thePos]},
(*Print["about to use myExpect in func:",{compCon,stateSel,xtm1,Identity[Identity[With[{hoop=(qrSubbed&[tryEps])/.lucaSubs},hoop]]],tryEps,stdev,qrSubbed}];*)
myExpect[compCon,stateSel,Identity[Identity[With[{hoop=(qrSubbed&[tryEps])/.lucaSubs},hoop]]],tryEps,stdev]]}},(*Print["done use myExpect"];*)
(*Print["reFunc=",reFunc//InputForm];*)
With[{whl={#,reFunc @@ #}& /@
thePts},(*Print["done making whl"];*)
doScalarIntegration[whl,#,iOrder]&@pos]]/;
With[{theRes=theFunc[0,0,0,1]},Print["iPtsRE:theRes=",theRes];
NumberQ[theRes]]



Print["try reusing modSpecific in definition"]
myExpect[
modSpecific:compCon:{_Function...},stateSel_List,aFuncNow:fpForInitStateFunc[{compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
{qVal_?NumberQ,ruVal_?NumberQ,epsVal_},
zFuncs_List,pos_Integer],aVar_,stdev_?NumberQ]:=
Module[{},(*Print["myExpect:",{aFunc,aVar,aFuncNow,stdev}//InputForm];*)
If[stdev==0,(*Print["aFunc subbed:",{aFunc/.aVar->0,aFuncNow/.aVar->0}];*)aFuncNow/.aVar->0,
With[{theIntBody=({aFuncNow*PDF[NormalDistribution[0,stdev],tryEps],{aVar,-4*sigma$u,4*sigma$u}(*,
AccuracyGoal -> 2, Compiled -> Automatic,
  PrecisionGoal -> 2, WorkingPrecision -> 2*)}/.lucaSubs)},(*Print["myExpect:intBody=",theIntBody//InputForm];*)
NIntegrate @@ theIntBody]]]



iterPF[{
modSpecific:compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
iOrder_Integer,gSpec:{qPts_Integer,rPts_Integer},zFuncsNow_List]:=With[
{fpSolnFunc=Function[{xx,yy,zz},fpForInitStateFunc[{compCon,stateSel,xtm1},
{xx,yy,zz},zFuncsNow]]},
makeInterpFuncPF[{compCon,stateSel,xtm1},fpSolnFunc,iOrder,
{{qPts,qLow//.lucaSubs//N,qHigh//.lucaSubs//N},
{rPts,ruLow//.lucaSubs//N,ruHigh//.lucaSubs//N}}]]/;
And[iOrder>=0,Min[gSpec]>=iOrder]

iterRE[{
modSpecific:compCon:{_Function...},stateSel_List,xtm1_?MatrixQ},
iOrder_Integer,pSpec:{qPts_Integer,rPts_Integer,ePts_Integer},
zFuncsNow_List,stdev_?NumberQ]:=
With[{gSpec=
{{qPts,qLow//.lucaSubs//N,qHigh//.lucaSubs//N},
{rPts,ruLow//.lucaSubs//N,ruHigh//.lucaSubs//N}(*,
{ePts,-2*sigma$u//.lucaSubs//N,2*sigma$u//.lucaSubs//N}*)},
agedZs=ageZFuncs[zFuncsNow],
fpSolnFunc=Function[{xx,yy,zz,pos},
fpForInitStateFunc[{compCon,stateSel,xtm1},
{xx,yy,zz},zFuncsNow,pos]]},
With[
{forQ=makeInterpFuncRE[{compCon,stateSel,xtm1},fpSolnFunc,1,iOrder,gSpec,stdev],
forRu=makeInterpFuncRE[{compCon,stateSel,xtm1},fpSolnFunc,2,iOrder,gSpec,stdev],
forNewZ=makeInterpFuncRE[{compCon,stateSel,xtm1},fpSolnFunc,-1,iOrder,gSpec,stdev]},
With[{newRes=Join[{forQ,forRu},agedZs,{forNewZ}]},newRes]]]/;
And[iOrder>=0,Min[pSpec]>=iOrder]


aPathNoCnstrn[
qtm1Arg_?NumberQ,rutm1Arg_?NumberQ,epsArg_?NumberQ,pad_Integer:1]:=  
With[{epsImpact=Flatten[phimat . psieps *epsArg+
Inverse[IdentityMatrix[3]-fmat] . phimat . psic ]//.lucaSubs,
pathIterFunc=Function @@{{qq,rr,ru},Flatten[
((bmat . {{qq},{rr},{ru}})+  Inverse[IdentityMatrix[3]-fmat] . phimat . psic )]//.lucaSubs}},
With[{pathVals=NestList[pathIterFunc @@ #&,
(pathIterFunc @@ {qtm1Arg,rr,rutm1Arg})+epsImpact,pad-1]},
Join[{{qtm1Arg},{ignored},{rutm1Arg}},Transpose[{Flatten[pathVals]}]]]]/;pad>0


hmatApp[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,zFuncs_List]:=
With[{tp=aPath[qtm1,rutm1,eps,zFuncs]},
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar/.lucaSubs),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic//.lucaSubs))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]]


hmatAppFinal[qtm1_?NumberQ,rutm1_?NumberQ,eps_?NumberQ,finalFuncs_List]:=
With[{tp=aPathFinal[qtm1,rutm1,eps,finalFuncs]},
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar/.lucaSubs),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic//.lucaSubs))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]]

hmatAppGeneric[tp_?MatrixQ,eps_:0]:=
With[{tpPers=Length[tp]/3},
With[{epsVecs=Partition[Transpose[{ReplacePart[Table[0,{3*((tpPers)-2)}],2->eps]}],3],
rDiffVec=Partition[Drop[Drop[(tp-rUnderBar/.lucaSubs),-3],3] * 
Flatten[Table[{{0},{1},{0}},{tpPers-2}],1],3]
},
With[{hApps=((hmat.tp[[Range[9]+3*#]])-(psic//.lucaSubs))&/@Range[0,(tpPers)-3]},
Flatten[rDiffVec *(epsVecs-(Join@ hApps//Chop))]]]]






End[]

EndPackage[]
Print["done reading lucaSimpleModel.m"]
