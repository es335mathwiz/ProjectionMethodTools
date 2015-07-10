Print["reading regimeSwitchingRBC.m"]
BeginPackage["regimeSwitchingRBC`",{"occBindRecur`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","ProjectionInterface`"}]

cc::usage="rbc model variable"
kk::usage="rbc model variable"
zz::usage="rbc model variable"
alpha::usage="rbc model parameter"
delta::usage="rbc model parameter"
rho::usage="rbc model parameter"
mu::usage="rbc model parameter"
sigma::usage="rbc model parameter"
nu::usage="rbc model parameter"
beta::usage="rbc model parameter"

Begin["Private`"]

(*pg 6 of  forerster, rubio-ramirez, waggoner and zha*)
rbcEqns={
 cc[t]^(nu-1) -(
beta  *(zz[t]^(nu-1))*(cc[t+1]^(nu-1))*(
(alpha E^((zz[t+1]^(1-alpha)))*(kk[t]^(alpha-1)) + (1-delta) ))),
cc[t] + zz[t]*kk[t] -( (zz[t]^(1-alpha))* (kk[t-1]^alpha) + (1-delta)*kk[t-1]),
Log[zz[t]] -( (1-rho[state[t]])*mu[state[t]] + rho[state[t]]*Log[zz[t-1]] + 
sigma[state[t]]* eps[zz][t] )
}
(*parameters page 21 using state 1*)
paramSubs01={
alpha->33/100,beta->9976/10000,nu->-1,delta->25/1000,
mu->274/10000,
rho->1/10,
sigma->72/10000
}

(*parameters page 21 not using state 2*)
paramSubs02={
alpha->33/100,beta->9976/10000,nu->-1,delta->25/1000,
mu->337/10000,
rho->0,
sigma->216/10000
}

pSubs={p11->75/100,p22->5/10}

pmat={{p11,1-p11},{1-p22,p22}}/.pSubs;
xVec={{x1,1-x1}};
xSoln=Flatten[Solve[Thread[Flatten[xVec . pmat] == Flatten[xVec]],x1]]

pBar={{x1},{1-x1}}/.xSoln
theta01={mu,rho,sigma}/.paramSubs01;
theta02={mu,rho,sigma}/.paramSubs02;
thetaBar=Flatten[Transpose[{theta01,theta02}] . pBar]

paramErgodic={
alpha->33/100,beta->9976/10000,nu->-1,delta->25/1000,
mu->thetaBar[[1]],
rho->thetaBar[[2]],
sigma->thetaBar[[3]]
}


paramSubs=paramSubs01;


simpSubs={rho[state[t]]->rho,mu[state[t]]->mu,sigma[state[t]]->sigma}


rbcSimp01=(rbcEqns/.simpSubs)/.paramSubs01;
rbcSimp02=(rbcEqns/.simpSubs)/.paramSubs02;
rbcSimpErg=(rbcEqns/.simpSubs)/.paramErgodic;

rbcSSEqns01=Thread[(rbcSimp01/.{eps[_][_]->0,xx_[t+_.]->xx})==0]
rbcSSEqns02=Thread[(rbcSimp02/.{eps[_][_]->0,xx_[t+_.]->xx})==0]
rbcSSEqnsErg=Thread[(rbcSimpErg/.{eps[_][_]->0,xx_[t+_.]->xx})==0]


ssSoln01=FindRoot[rbcSSEqns01,{{cc,1},{kk,1},{zz,1}}]
ssSoln02=FindRoot[rbcSSEqns02,{{cc,1},{kk,1},{zz,1}}]
ssSolnErg=FindRoot[rbcSSEqnsErg,{{cc,1},{kk,1},{zz,1}}]



hmatSymbPre=equationsToMatrix[
rbcSimpErg/.{eps[_][_]->0}]//FullSimplify;
hmat=(hmatSymbPre/.xx_[t+_.]->xx)/.ssSolnErg;
{ig, ig, ig, ig, qmat, ig, ig, ig} = NumericAMA`numericAMA[hmat, 1, 1]; 
{bmat, phimat, fmat} = NumericAMA`numericComputeBPhiF[hmat,qmat] 

(*

{zfSymb,hfSymb}=symbolicAR[hmatSymb];

amatSymb=symbolicTransitionMatrix[hfSymb];

{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{5}]]];
{bmatSymb,phimatSymb,fmatSymb}=symbolicComputeBPhiF[hmatSymb,qmatSymb]//FullSimplify;
*)
End[]
EndPackage[]
Print["done reading regimeSwitchingRBC.m"]
