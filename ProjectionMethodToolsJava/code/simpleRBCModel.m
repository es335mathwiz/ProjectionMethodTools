PrependTo[$Path,"../../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading simpleRBCModel.m"]
BeginPackage["simpleRBCModel`",{"AMASeriesRepresentation`",(*"occBindRecur`",*)"ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"(*,"ProjectionInterface`"*)}]

ratioThetaToC::usage="rbc model variable"
cc::usage="rbc model variable"
kk::usage="rbc model variable"
rho::usage="rbc model parameter"
theta::usage="rbc model parameter"
alpha::usage="rbc model parameter"
delta::usage="rbc model parameter"
sigma::usage="rbc model parameter"
rbcEqns::usage="rbc model equations"
simpParamSubs::usage="simpParamSubs=Join[paramSubs,forParamSubs]"
ssSolnSubsRE::usage="rational expectations steady state"
ssSolnSubsPF::usage="perfect foresight steady state"
condExpRE::usage="condExpRE[kktm1_?NumberQ,ii_Integer]"
condExpPF::usage="condExpPF[kktm1_?NumberQ,ii_Integer]"
compApproxRE::usage="compApproxRE[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},kk_,theta_,epsNow_,iters_Integer]"
compApproxDiffRE::usage="compApproxDiffRE[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},kk_,theta_,epsNow_,iters_Integer]"
maxZsRE::usage="maxZsRE[theHmat_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?Matrix,{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]"

compBounds::usage="compBounds[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]"

Begin["Private`"]








(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
 1/cc[t]-(delta*((theta[t+1])*(1/cc[t+1])*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
(*Log[theta[t]]-rho*Log[theta[t-1]] - eps[theta][t]*)
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}
(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100
} 

forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)}
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs
simpParamSubs=Join[paramSubs,forParamSubs]


If[Length[ssSolnSubsRE]===0,
Print["computing steady state subs"];
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
Print[thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta]][[2]]];
Print[kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk]][[-1]]];
Print[cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE)];
Print[ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE}]];
anExpPF=1;
Print[thSubsPF=Flatten[Solve[theta==anExpPF*theta^rho,theta]][[1]]];
Print[kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]]];
Print[cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF)];
Print[ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF}]];
Print["done computing steady state subs"];
]




psiz=IdentityMatrix[3]

Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xx_[t+_.]->xx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xx_[t+_.]->xx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[3]]]+hmatSymbRE[[All,3+Range[3]]]+hmatSymbRE[[All,6+Range[3]]];

ssSolnVecRE={{cc},{kk},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;



(*made up c value =1*)
(*
condExpRE[cctm1_,kktm1_,thtm1_,epsVal_,
ii_Integer]:=
With[{thVals=Join[{},Drop[NestList[(anExpRE*thNow[#,0])&,(thNow[thtm1,epsVal]),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=(Drop[yyVals,0]-Drop[kkVals,1])},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]
*)

condExpRE=Compile[
{{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},{ii,_Integer}},
With[{thVals=Join[{},
Drop[NestList[(anExpRE*thNow[#,0])&,
(thNow[thtm1,epsVal]),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=(Drop[yyVals,0]-Drop[kkVals,1])},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]]


(*made up c value =1*)(*
genZsRE[{anHmat_?MatrixQ,aPsiEps_?MatrixQ,aPsiC_?MatrixQ},
cc_,kk_,theta_,epsNow_,iters_Integer]:=
With[{rbcPath=Flatten[condExpRE[cc,kk,theta,epsNow,iters+1]]},
With[{begi=
(anHmat). (Transpose[{rbcPath[[Range[9]]]}]) -(aPsiC)-aPsiEps. {{epsNow}}},
With[{along=((anHmat) . (Transpose[{rbcPath[[#*3+Range[9]]]}]) -(aPsiC//N)) &/@
Range[iters-1]},Partition[Join[begi,Join @@along],3]]]]
*)

genZsRE=Compile[{{anHmat,_Real,2},{aPsiEps,_Real,2},{aPsiC,_Real,2},
{cc, _Real},{kk, _Real},{theta,_Real},{epsNow, _Real},{iters,_Integer}},
Module[{},
With[{rbcPath=Flatten[condExpRE[cc,kk,theta,epsNow,iters+1]]},
With[{begi=
(anHmat). (Transpose[{rbcPath[[Range[9]]]}]) -(aPsiC)-aPsiEps. {{epsNow}}},
If[iters==1,{begi},
With[{along=((anHmat) . (Transpose[{rbcPath[[#*3+Range[9]]]}]) -(aPsiC//N)) &/@
Range[iters-1]},
With[{theRes=doJoin[begi,along]},
theRes]]]]]],
{{condExpRE[___],_Real,2},{doJoin[___],_Real,3}}]




doJoin[begi_?MatrixQ,along:{_?MatrixQ..}]:=
Partition[Join[begi,Join @@along],3]



(*made up c value =1*)
compApproxRE[theHmat_?MatrixQ,
linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
cc_,kk_,theta_,epsNow_,iters_Integer]:=
Module[{},
(*Print["compApproxRE:",{phi,FF,psiZ,{theHmat,psiEps,psiC,cc,kk,theta,epsNow,iters}}];
Print["huh:",genZsRE[theHmat,psiEps,psiC,cc,kk,theta,epsNow,iters]];*)
With[{fPart=
fSumC[phi,FF,psiZ,genZsRE[theHmat,psiEps,psiC,cc,kk,theta,epsNow,iters]]},(*Print["fPart"];*)
(*Print[{fPart,BB . Transpose[{{1,kk,theta}}] , phi . psiEps . {{epsNow}} ,Inverse[IdentityMatrix[3] - FF] . phi . psiC ,genZsRE[theHmat,psiEps,psiC,cc,kk,theta,epsNow,iters]}];*)
BB . Transpose[{{1,kk,theta}}] + phi . psiEps . {{epsNow}} + 
Inverse[IdentityMatrix[3] - FF] . phi . psiC +
fPart]]


compApproxDiffRE[theHmat_?MatrixQ,
linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
cc_,kk_,theta_,epsNow_,iters_Integer]:=
Module[{},(*Print["compApproxDiffRE:"];*)
With[{theApprox=compApproxRE[theHmat,
linMod,cc,kk,theta,epsNow,iters],
exact=condExpRE[1,kk,theta,epsNow,1][[{4,5,6}]]
},theApprox-exact]]



compBounds[theHmat_?MatrixQ,
linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},rngs:{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]:=
With[{bigZ = maxZsRE[theHmat, psiEps, 
   psiC, rngs, iters],
matPart = 
  Norm[truncErrorMat[FF, phi, iters - 1], Infinity]},(*Print["compBounds mid"];*)
{bigZ, matPart, bigZ[[1]]*matPart, 
 infNorm[compApproxDiffRE[theHmat, 
    linMod, #1, #2, #3, #4, iters] &, rngs]}]


maxZsRE[theHmat_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,
{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]:=
infNorm[genZsRE[theHmat,psiEps,psiC,#1,#2,#3,#4,iters][[-1]]&,
{{lowc,highc},{lowk,highk},{lowt,hight},{lowe,highe}}]



infNorm[func_,{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}}]:=
With[{blkEvalFunc=Unique["blkFunc"]},
blkEvalFunc[cc_?NumberQ, kk_?NumberQ, tt_?NumberQ, ee_?NumberQ]:= 
func[cc,kk,tt,ee];
With[{first=
(NMaximize[
{Norm[blkEvalFunc[cc,kk,tt,ee],Infinity],(lowc<=cc<=highc)&&(lowk<=kk<=highk)&&(lowt<=tt<=hight)&&(lowe<=ee<=highe)},{cc,kk,tt,ee}(*,EvaluationMonitor:>Sow[{cc,kk,tt,ee,func[cc,kk,tt,ee],Norm[func[cc,kk,tt,ee],Infinity]}]*),MaxIterations->150]
)},first]]


Print["PF solutions soon"]





rbcEqnsFunctionalNext=Compile[
{
{ctm1,_Real},{kktm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}]


(*

(*
eqnErrsOnPathPF::usage="eqnErrsOnPathPF[drFunc_Function]"
condExpExt::usage="condExpExt"
iterateDRPF::usage="iterateDRPF[drFunc_Function,initVec:{ig_,initK_?NumberQ,ig_,initTh_?NumberQ,initEps_?NumberQ},numPers_Integer]"
*)
compCon::usage="compCon[aPath_?MatrixQ]:=Function[{aPath,theZs}"
condExp::usage="condExp[kktm1_?NumberQ,ii_Integer]"
fixCondExp::usage="condExp[kktm1_?NumberQ,ii_Integer]"
compZs::usage="compZs[hmatNum_?MatrixQ,kktm1_?NumberQ,ii_Integer]"





rbcEqnsExt={
  1/cc[t]-(delta*((ratioThetaToC[t+1])*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
(* Log[theta[t]]-rho*Log[theta[t-1]] - eps[theta][t],*)
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
 ratioThetaToC[t]-(theta[t]/cc[t])
}














condExpRE=
Compile[{{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},
{ii,_Integer}},
With[{thVals=Join[{},Drop[NestList[E^(((sigma^2)/2))*thNow[#]&,(E^epsVal)*thNow[thtm1],ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=Drop[yyVals,0]-Drop[kkVals,1]},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]//.simpParamSubs]]]]]]



(*


If[Length[cSSSub]===0,
Print["computing steady state subs"];
rbcSSEqns=Thread[(rbcEqns//.{eps[_][_]->0,xx_[t+_.]->xx})==0];
kSSSub=PowerExpand[Simplify[Solve[delta*alpha*kk^alpha==kk,{kk},Reals],(0<alpha<1)&&(0<delta<1)][[2]]];
cSSSub=Flatten[Solve[Simplify[rbcSSEqns//.kSSSub][[2]],cc]];
ssSolnSubs=Join[cSSSub,kSSSub,{theta->1,ratioThetaToC->1/cc}];
rbcSSEqns=Thread[(rbcEqns//.{eps[_][_]->0,xx_[t+_.]->xx})==0];
Private`rbcSSThetaREEqn=Solve[(Log[theta]==Mean[LogNormalDistribution[0,sigma/.Private`paramSubs]])//.Private`simpParamSubs,theta]
]
((alpha*delta)//.simpParamSubs//N)*thNow*lastK^(alpha//.simpParamSubs//N)
*)

hmatSymbExt=hmatSymbRawExt//.simpSubs
hSumExt=hmatSymbExt[[All,Range[4]]]+hmatSymbExt[[All,4+Range[4]]]+hmatSymbExt[[All,8+Range[4]]];
ssSolnVecExt={{cc},{kk},{ratioThetaToC},{theta}}//.ssSolnSubs;
psicSymbExt=hSumExt . ssSolnVecExt;
psicExt=psicSymbExt//.simpParamSubs


{zfSymb,hfSymb}=symbolicAR[hmatSymb//.simpParamSubs];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{1}]]];



		      
{zfSymbExt,hfSymbExt}=symbolicAR[hmatSymbExt//.simpParamSubs];
amatSymbExt=symbolicTransitionMatrix[hfSymbExt];
{evlsSymbExt,evcsSymbExt}=Eigensystem[Transpose[amatSymbExt]];
qmatSymbExt=Join[zfSymbExt,evcsSymbExt[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]


hmat=hmatSymb//.simpParamSubs;
qmat=qmatSymb//.simpParamSubs;
Print["computing and simplifying the symbolic b phi f etc"]
{bmat,phimat,fmat}=symbolicComputeBPhiF[hmat,qmat]//Simplify;


hmatExt=hmatSymbExt//.simpParamSubs;
qmatExt=qmatSymbExt//.simpParamSubs;
Print["computing and simplifying the symbolic b phi f etc"]
{bmatExt,phimatExt,fmatExt}=symbolicComputeBPhiF[hmatExt,qmatExt]//Simplify;




Print["defining constraint and selector funcs"]


compCon={
Function[{aPath,theZs},0==rbcSimp[[1]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[5,1]],
theta[t-1]->aPath[[3,1]],theta[t]->aPath[[6,1]],
cc[t]->aPath[[4,1]],cc[t+1]->aPath[[7,1]],eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimp[[2]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[5,1]],
theta[t-1]->aPath[[3,1]],theta[t]->aPath[[6,1]],
cc[t]->aPath[[4,1]],cc[t+1]->aPath[[7,1]],eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimp[[3]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[5,1]],
theta[t-1]->aPath[[3,1]],theta[t]->aPath[[6,1]],
cc[t]->aPath[[4,1]],cc[t+1]->aPath[[7,1]],eps[theta][t]->eps}]
}

compConExt={
Function[{aPath,theZs},0==rbcSimpExt[[1]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[6,1]],
theta[t-1]->aPath[[4,1]],theta[t]->aPath[[8,1]],
cc[t]->aPath[[5,1]],cc[t+1]->aPath[[9,1]],
ratioThetaToC[t]->aPath[[7,1]],ratioThetaToC[t+1]->aPath[[11,1]],
eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimpExt[[2]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[6,1]],
theta[t-1]->aPath[[4,1]],theta[t]->aPath[[8,1]],
cc[t]->aPath[[5,1]],cc[t+1]->aPath[[9,1]],
ratioThetaToC[t]->aPath[[7,1]],ratioThetaToC[t+1]->aPath[[11,1]],
eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimpExt[[3]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[6,1]],
theta[t-1]->aPath[[4,1]],theta[t]->aPath[[8,1]],
cc[t]->aPath[[5,1]],cc[t+1]->aPath[[9,1]],
ratioThetaToC[t]->aPath[[7,1]],ratioThetaToC[t+1]->aPath[[11,1]],
eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimpExt[[4]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[6,1]],
theta[t-1]->aPath[[4,1]],theta[t]->aPath[[8,1]],
cc[t]->aPath[[5,1]],cc[t+1]->aPath[[9,1]],
ratioThetaToC[t]->aPath[[7,1]],ratioThetaToC[t+1]->aPath[[11,1]],
eps[theta][t]->eps}]
}



(*always used with epsVal=0*)
noCnstrnGuess= With[{linPFSys=
Flatten[bmat . {{0},{kVal},{tVal}}+phimat . (psieps*epsVal+psic)]//.paramSubs},
{Function @@ {{kVal,tVal,epsVal},Flatten[linPFSys[[{2,3}]]]}}]

(*/.paramSubs//myN;*)

sigVal=sigma//.simpParamSubs//N;
rhoVal=rho//.simpParamSubs//N;
(*
condExp=
Compile[{{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},
{ii,_Integer}},
With[{thVals=Join[{},Drop[NestList[E^(rhoVal*Log[#])&,E^(rhoVal*Log[thtm1]+epsVal),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=Drop[yyVals,0]-Drop[kkVals,1]},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]]
*)


condExpExt=
Compile[{{cctm1,_Real},{kktm1,_Real},
{ccinv,_Real},{thtm1,_Real},{epsVal,_Real}},
condExp[cctm1,kktm1,thtm1,epsVal,1]]

fixCondExp[cctm1_,kktm1_,thtm1_,epsVal_,ii_]:=
With[{thVals=Join[{},Drop[NestList[thNow,(E^epsVal)*thNow[thtm1],ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=Drop[yyVals,0]-Drop[kkVals,1]},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]

compZs=Compile[{{tryMat,_Real,2},{phimat,_Real,2},{psieps,_Real,2},{psic,_Real,2},{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},{ii,_Integer}},
With[{thePath=Flatten[condExp[cctm1,kktm1,thtm1,epsVal,ii+2]]},
With[{raw=Flatten[
Table[(tryMat . Transpose[{thePath[[jj*3+Range[9]]]}])-psic,{jj,0,ii-1}]]},
With[{epsPart=PadRight[Flatten[-(psieps *epsVal)],Length[raw]]},
raw+epsPart
]]]]






rbcEqnsApply[{ctm1_,ktm1_,ct_,kt_,ctp1_,ktp1_}]:=
rbcEqns/.eps[theta][t]->0/.{cc[t]->ct,cc[t+1]->ctp1,kk[t-1]->ktm1,kk[t]->kt}//.simpParamSubs



(*c,k,theta*)
(*
[{{ctm1_},{ktm1_},{thtm1_}},{{ct_},{kt_},{tht_}},{{ctp1_},{ktp1_},{thtp1_}}]
*)


hFunc=Function[{xtm1,xt,xtp1,epst},
rbcSimp/.
{cc[t-1]->xtm1[[1,1]],kk[t-1]->xtm1[[2,1]],theta[t-1]->xtm1[[3,1]],
cc[t]->xt[[1,1]],kk[t]->xt[[2,1]],theta[t]->xt[[3,1]],
cc[t+1]->xtp1[[1,1]],kk[t+1]->xtp1[[2,1]],theta[t+1]->xtp1[[3,1]],
eps[theta][t]->epst[[1,1]]}]


hFuncExt=Function[{xtm1,xt,xtp1,epst},
rbcSimpExt/.
{cc[t-1]->xtm1[[1,1]],kk[t-1]->xtm1[[2,1]],ratioThetaToC[t-1]->xtm1[[3,1]],theta[t-1]->xtm1[[4,1]],
cc[t]->xt[[1,1]],kk[t]->xt[[2,1]],ratioThetaToC[t]->xt[[3,1]],theta[t]->xt[[4,1]],
cc[t+1]->xtp1[[1,1]],kk[t+1]->xtp1[[2,1]],ratioThetaToC[t+1]->xtp1[[3,1]],theta[t+1]->xtp1[[4,1]],
eps[theta][t]->epst[[1,1]]}]



(*prep rbcEqnsFunctional*)
(*
lookey=(((Private`rbcEqnsExt//.Private`simpParamSubs)//myN)/.
{
cc[t-1]->cctm1,kk[t-1]->kktm1,ratioThetaToC[t-1]->ratiotm1,theta[t-1]->thetatm1,
cc[t]->cct,kk[t]->kkt,ratioThetaToC[t]->ratiot,theta[t]->thetat,
cc[t+1]->cctp1,kk[t+1]->kktp1,ratioThetaToC[t+1]->ratiotp1,theta[t+1]->thetatp1,
eps[theta][t]->epsVal})//InputForm
{cct^(-1) - (0.342*ratiotp1)/kkt^(16/25), cct + kkt - 1.*kktm1^(9/25)*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20), 
 ratiot - (1.*thetat)/cct}

*)


rbcEqnsFunctional=Compile[
{
{ctm1,_Real},{kktm1,_Real},{ratiotm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{ratiot,_Real},{thetat,_Real},
{ctp1,_Real},{kktp1,_Real},{ratiotp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*ratiotp1)/kkt^(16/25), cct + kkt - 1.*kktm1^(9/25)*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20), 
 ratiot - (1.*thetat)/cct}]


rbcEqnsFunctionalNext=Compile[
{
{ctm1,_Real},{kktm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}]


rbcEqnsFunctionalNext=Compile[
{
{ctm1,_Real},{kktm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}]







iterateDRPF[drFunc_Function,
initVec:{ig1_,initK_,ig2_,initTh_,initEps_},
numPers_Integer]:=
With[{firVal=drFunc[initVec]},
With[{theRes=NestList[drFunc[Append[#[[Range[4]]],0]]&,firVal,numPers-1]},
theRes]]/;
And[numPers>0]


iterateExact[
initVec:{ig1_,initK_,ig2_,initTh_,initEps_},
numPers_Integer]:=
With[{firVal=condExpExt @@ initVec},
With[{theRes=NestList[(condExpExt[Sequence[#],1]& @@ Append[Flatten[#[[Range[4]]]],0])&,firVal,numPers-1]},
theRes]]/;
And[numPers>0]




eqnErrsOnPathPF[drFunc_Function,
initVec:{ig1_,initK_,ig2_,initTh_,initEps_},
numPers_Integer]:=
With[{theVarsPath=Take[#,4]&/@iterateDRPF[drFunc,initVec,numPers+1]},
With[{theSimPath=
Join[{
Append[Transpose[{Drop[initVec,-1]}],{0}],
Append[Transpose[{theVarsPath[[1]]}],{initVec[[-1]]}]},
Append[Transpose[{#}],{0}]& /@Drop[theVarsPath,1]
]},
{theSimPath,Table[hFuncExt[theSimPath[[ii]],theSimPath[[ii+1]],theSimPath[[ii+2]],theSimPath[[ii+1,{-1}]]],{ii,1,numPers}]}]
]

(*


iterateDR[drFunc_Function,
initVec:{initK_?NumberQ,initTh_?NumberQ,initEps_?NumberQ},
expctSpec:{{anEpsVar_,aDist_},opts_:{}},numPers_Integer,reps_Integer:1]:=
With[{firVal=drFunc @@ initVec},
With[{allReps=
Table[
NestList[drFunc @@ {#[[1]],#[[3]],
If[NumberQ[aDist],aDist,RandomVariate[aDist]]}&,firVal,numPers-1],{reps}]},
With[{theMean=prepMeansForHApp[Mean[allReps],initVec]},
If[reps==1,theMean,
{theMean,prepStdDevsForHApp[StandardDeviation[allReps]]}]]]]/;
And[reps>0,numPers>0]
*)

*)
End[]
EndPackage[]
Print["done reading simpleRBCModel.m"]

