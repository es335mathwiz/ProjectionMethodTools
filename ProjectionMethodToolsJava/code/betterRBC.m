PrependTo[$Path,"../../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading betterRBC.m"]
BeginPackage["betterRBC`",{"AMASeriesRepresentation`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"}]

		
betterExactDR::usage= "betterExactDR"
betterRBCExactCondExp::usage = "makeREIterFunc[betterRBCExactDR,theDist]"

(*
condExpRE::usage="condExpRE[kktm1_?NumberQ,ii_Integer]"
condExpREFunc::usage="condExpRE[kktm1_?NumberQ,ii_Integer]"
lilCondExpREFunc::usage="condExpRE[kktm1_?NumberQ,ii_Integer]"
*)
theDist::usage="charactizes the distribution of epsilon"
linMod::usage="linear model matrices for approx"
rbcEqnsCompiled::usage="compiled version of model equations"
ssSolnSubsRE::usage="solns subs for steady state"
ssSolnSubsPF::usage="solns subs for steady state"
simpParamSubs::usage="simpParamSubs=Join[paramSubs,forParamSubs]"


cc::usage="rbc model variable"
kk::usage="rbc model variable"
nl::usage="rbc model variable"
rho::usage="rbc model parameter"
theta::usage="rbc model parameter"
alpha::usage="rbc model parameter"
delta::usage="rbc model parameter"
sigma::usage="rbc model parameter"


Begin["Private`"]



CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}




(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];


rbcCompileGuts=(rbcEqns/.{
cc[t-1]->cctm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thtm1,
cc[t]->cct,kk[t]->kkt,nlPart[t]->nlt,theta[t]->tht,
cc[t+1]->cctp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thtp1,
eps[theta][t]->epsVal
}//.paramSubs)//N

rbcEqnsCompiled=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.)*(nltp1(*thetatp1/cctp1*))))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}}


Needs["CompiledFunctionTools`"]





(*If[Length[ssSolnSubsRE]===0,*)
Print["computing steady state subs"];
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];
Print["done computing steady state subs"];
(*]*)

(*
condExpPF=Compile[
{{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thtm1,_Real},{epsVal,_Real},{ii,_Integer}},
With[{thVals=Join[{},
Drop[NestList[(anExpPF*thNow[#,0])&,
(thNow[thtm1,epsVal]),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=(Drop[yyVals,0]-Drop[kkVals,1])},
With[{thetransp=Partition[Flatten[Transpose[
{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}
]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]]
*)



(*
condExpRE=Compile[
{{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thtm1,_Real},{epsVal,_Real},{ii,_Integer}},
With[{thVals=Join[{thtm1},
Drop[NestList[(anExpRE*thNow[#,0])&,
(thNow[thtm1,epsVal]),ii+1],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],-1]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,-1]}]},
With[{ccVals=(Drop[yyVals,0]-Drop[kkVals,1])},
With[{nlVals= (1/ccVals)},
With[{thetransp=Partition[Flatten[Transpose[
{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[nlVals],Flatten[Drop[thVals,1]]}]],1]},
Join[{{cctm1},{kktm1},{nltm1},{thtm1}},Drop[thetransp,-4]]]]]]]]]

*)
betterExactDR = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]



theDist={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;



betterRBCExactCondExp = makeREIterFunc[betterRBCExactDR,theDist]


psiz=IdentityMatrix[4]

Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];

ssSolnVecRE={{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linMod={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};

(*


hmatLilSymbRE=hmatSymbRE[[gInd,Join[gInd,gInd+3,gInd+6]]]

hSumRE=hmatLilSymbRE[[All,Range[3]]]+hmatLilSymbRE[[All,3+Range[3]]]+hmatLilSymbRE[[All,6+Range[3]]];

ssSolnVecRE={{cc},{kk},{theta}}//.ssSolnSubsRE;
psicLilSymbRE=hSumRE . ssSolnVecRE;


{zfLilSymbRE,hfLilSymbRE}=symbolicAR[hmatLilSymbRE//.simpParamSubs];
amatLilSymbRE=symbolicTransitionMatrix[hfLilSymbRE];
{evlsLilSymbRE,evcsLilSymbRE}=Eigensystem[Transpose[amatLilSymbRE]];
qmatLilSymbRE=Join[zfLilSymbRE,evcsLilSymbRE[[{1}]]];



lilLinMod={bmatLilSymbRE[[gInd,gInd]] // N, phimatLilSymbRE[[gInd,gInd]] // N, 
    fmatLilSymbRE[[gInd,gInd]] // N, psiepsLilSymbRE[[gInd]] // N, 
    psicLilSymbRE[[gInd]] // N, psiz[[gInd,gInd]] // N,{{0}}};

*)

gInd={1,2,4};
(*
lilCondExpREFunc = 
 Function[{cc, kk,  th, eps},
condExpREFunc[cc,kk,1,th,eps][[gInd]]]
*)

X0Z0=genX0Z0Funcs[linMod];


End[]
EndPackage[]
Print["done reading betterRBC.m"]
