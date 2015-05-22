PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
BeginPackage["occBindRecur`",{"ProtectedSymbols`","ProjectionInterface`","JLink`","AMAModel`","NumericAMA`"}]
genCompSlackSysFunc::usage="genCompSlack[pathLen_Integer,zFuncs:{_Function...}]"
fpForInitStateFunc::usage="fpForInitVecFunc[qVal,ruVal,epsVal,theCompSlackSysFunc_,zFuncs:{_Function...}]"

numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"
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
rho$ru::usage="parameter for model"
adj::usage="parameter for model"
uu::usage="parameter for model"
phip::usage="parameter for model"
rUnderBar::usage="parameter for model"
Protect[qq,rr,ru,rhop,betap,sigmap,rho$ru,adj,uu,phip,rUnderBar]

(*assigned Private`ly below*)
hmat::usage="simple model matrix"
qmat::usage="simple model matrix"
bmat::usage="simple model matrix"
phimat::usage="simple model matrix"
fmat::usage="simple model matrix"
psiz::usage="simple model matrix"
psieps::usage="simple model matrix"


numIt::usage="numIt[xx_]  does lucaSubs and leaves t values untouched by N"






Begin["Private`"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]


numIt[xx_]:=xx//.lucaSubs//myN//Expand//Chop 
 


genCompSlackSysFunc[pathLen_Integer,zFuncs:{_Function...}]:=
With[{aPath=genPath[pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zFuncsApps=Drop[zFuncs[[-1]][qTry,rTry,0],2],
compCon=((aPath[[5,1]]>=0.02&&theZs[[-1]]==0)||
(aPath[[5,1]]==0.02&&theZs[[-1]]>=0))&&
qTry==aPath[[4,1]]&&
rTry==aPath[[6,1]]},
With[{zLeft=(Drop[theZs,-1])},
With[{zEqns=And @@ (Thread[zLeft==zFuncsApps])},
With[{theSys=And[compCon,zEqns]},
{zLeft,zEqns,zFuncsApps,compCon,theSys,
Function @@ {{qTry,rTry},
theSys}}]]]]]/;
And[
pathLen>0,
Length[zFuncs]===pathLen-1,
NumberQ[Plus @@ Flatten[Through[zFuncs[0,0,0]]]]]




fpForInitStateFunc[qVal_?NumberQ,ruVal_?NumberQ,epsVal_?NumberQ,
theCompSlackSysFunc_,zFuncs:{_Function...}]:=With[{
initGuess=If[True(*Length[zFuncs]==0*),
Flatten[bmat . {{qVal},{0},{ruVal}}+psieps.{{epsVal}}][[{1,3}]],
zFuncs[[-1]][qVal,ruVal,epsVal][[{1,2}]]],
rhsEqns=List @@ theCompSlackSysFunc[[2,{2,3},2]]/.{qtm1->qVal,rutm1->ruVal,eps->epsVal},
aPath=genPath[pathLen],
theZs=Flatten[genZVars[pathLen-1,1]]},
With[{zFuncsApps=Drop[zFuncs[[-1]][qTry,rTry,0],2],
initStateSubbed=
(theCompSlackSysFunc/.{qtm1->qVal,rutm1->ruVal,eps->epsVal})},
With[{zLeft=(Drop[theZs,-1])},
With[{zEqns=And @@ (Thread[zLeft==zFuncsApps])},
With[{theSys=And[compCon,zEqns]},
With[{zVars=Union[Cases[initStateSubbed,xx_[t],Infinity]]},
{zVars,rhsEqns,initGuess,initStateSubbed,
FixedPoint[Join[rhsEqns,zVars]/.Flatten[
NSolve[initStateSubbed @@ #,zVars]]&,initGuess]}]]]]]]

(*
test01=Function[{qtm1, rutm1, eps}, 
 Piecewise[{{0, eps + 0.433733789322437*qtm1 + 0.5000000000000001*rutm1 > 
     0.03239935157289748}, {0.0430556167081694 - 1.3289036544850497*eps - 
     0.5763904177042353*qtm1 - 0.664451827242525*rutm1, 
    eps + 0.433733789322437*qtm1 + 0.5000000000000001*rutm1 < 
     0.03239935157289748}}, 0]]


z01ExactFunc=Function @@ With[{eval=test01[qtm1,rutm1,eps]},{{qtm1,rutm1,eps},
Append[Flatten[(genPath[1][[{4,6}]])],Global`zzz$0$1[t]]/.
Global`zzz$0$1[t]->eval}]

{ig,ig,ig,ig,ig,ha2}=genCompSlackSysFunc[2,{z01ExactFunc}]

{zVars,rhsEqns,initGuess,initStateSubbed,fpnow}=fpForInitStateFunc[-.4,.1,0,ha2,{z01ExactFunc}]
*)

nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ]:=
bmat . xtm1 + phimat . psimat . epsilon


genPath[numNonZeroZs_Integer,padZeroZs_Integer]:=
With[{startPath=genPath[numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,psieps])//numIt)&,startPath[[{-3,-2,-1}]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]





genPath[numNonZeroZs_Integer]:=
With[{xtm1={{qtm1},{rtm1},{rutm1}},
rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,1,0] &/@Range[0,numNonZeroZs-1])//numIt]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,psieps]+rawFParts[[1]])//numIt},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,psieps]+#2//numIt)&,
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
   sigma$u -> 0.01, theMean -> {0}, rho$ru -> 0.5, adj -> 1};

   (*modParams = {betap, phip, rhop, rho$ru, sigmap} //. lucaSubs // N;*)
   modParams = {rUnderBar} //. lucaSubs // N;
Protect[lucaSubs]


lucaEqns = {qq[t] - (betap*(1 - rhop)*qq[t + 1] + rhop*qq[t - 1] - 
      sigmap*rr[t] + ru[t]),
   ru[t] - rho$ru*ru[t - 1] - adj*eps[uu][t],
   rr[t] - eqvdIf[phip*qq[t] >= rUnderBar, phip*qq[t], rUnderBar]}
Protect[lucaEqns]





{hmat,qmat,{bmat,phimat,fmat}}=numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)];
psiz={{0},{0},{1}};
psieps={{0},{1},{0}};
Protect[hmat,qmat,bmat,phimat,fmat,psiz,psieps]

End[]
EndPackage[]


