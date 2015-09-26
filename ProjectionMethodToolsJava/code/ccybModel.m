Print["reading ccybModel.m"]
BeginPackage["ccybModel`",{"occBindRecur`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","ProjectionInterface`"}]



compCon::usage="compCon[aPath_?MatrixQ]"
stateSel::usage="stateSel select state variables needed for iteration"
ccybEqns::usage="simple model equations"
ccybSubs::usage="typical simple model parameters"
beta::usage="typical simple model parameters"
rho::usage="typical simple model parameters"
theta::usage="typical simple model parameters"
kBar::usage="typical simple model parameters"
kLowBar::usage="typical simple model parameters"
delta::usage="typical simple model parameters"
omega::usage="typical simple model parameters"
sigma::usage="typical simple model parameters"
vv::usage="typical simple model parameters"
kk::usage="typical simple model parameters"
mu1::usage="typical simple model parameters"
mu2::usage="typical simple model parameters"
mu3::usage="typical simple model parameters"
getCcybFixedPoint::usage="getCcybFixedPoint[fpTarget_List,theSys_Function,initGuess_List]"



Protect[beta,rho,theta,kBar,delta,omega,sigma,vv,kk,mu1,mu2,mu3]
getCcybFixedPoint::usage="getCcybFixedPoint[fpTarget_List,theSys_Function,initGuess_List]"

numIt::usage="numIt[xx_]  does ccybSubs and leaves t values untouched by N[]"


Begin["Private`"]

numIt[xx_]:=xx//.ccybSubs//myN//Expand//Chop 
Print["defining ccyb model equations along with hmat, qmat, bmat, phimat and fmat"]

ccybSubs = {beta->99/100,rho->75/100,theta->0.5,kBar->2.5,kLowBar->0,
delta->0.1,
omega->((4/10)*delta)/(kBar*(1-beta*delta)*(1-theta)),sigma->2/10};


ccybEqns = {
vv[t] - mu1[t] + beta*rho*mu1[t+1],
omega*(kk[t]-theta*kk[t-1]) - delta*mu1[t] - mu2[t] + mu3[t] - 
beta*theta*omega*(kk[t+1]-theta*kk[t]),
vv[t] - rho*vv[t-1] + delta*kk[t] - eps[][t],
mu2[t],
mu3[t]
};
(*
kbar - k >= 0;
mu3 >= 0;
mu2 >= 0;
k >= 0;
*)
{hmat,qmat,{bmat,phimat,fmat}}=numericLinearizeSystemForOBC[
(ccybEqns/.{eps[][_]->0}//.(ccybSubs)//myN)]//Chop;


psic={{0},{0},{0},{0},{0}};
psiz={{0,0},{0,0},{0,0},{1,0},{0,1}};
psieps={{0},{0},{1},{0},{0}};

aPath01=genPath[{{kktm1},{mu1tm1},{mu2tm1},{mu3tm1},{vvtm1}},bmat,phimat,fmat,psieps,psic,psiz,2,1]//Chop


(*always used with epsVal=0*)
noCnstrnGuess= With[{linPFSys=
Flatten[bmat . {{kkVal},{0},{0},{0},{vvVal}}+phimat . (psieps *0+psic)]//.ccybSubs},
{Function @@ {{kkVal,vvVal},linPFSys[[1]]},
Function @@ {{kkVal,vvVal},linPFSys[[-1]]}}]

Print["defining constraint and selector funcs"]
(*aPath[[6,1]] is value of kk[t]*)

compCon={Function[{aPath,theZs},
((((aPath[[6,1]])>=kLowBar/.ccybSubs)&&theZs[[-2]]==0)||(((aPath[[6,1]])==kLowBar/.ccybSubs)&&theZs[[-2]]>=0&&theZs[[-1]]==0))],
Function[{aPath,theZs},((((aPath[[6,1]])<=kBar/.ccybSubs)&&theZs[[-1]]==0)||(((aPath[[6,1]])==kBar/.ccybSubs)&&theZs[[-1]]>=0&&theZs[[-2]]==0))]}




stateSel={1,5};

getCcybFixedPoint[fpTarget_List,theSys_Function,initGuess_List]:=
FixedPoint[fpTarget/.Flatten[With[{soln=
Identity[NSolve[(theSys @@ #),fpTarget,Reals]]},(*Print["soln=",{#,soln,fpTarget,theSys @@ #,theSys[[2]],theSys}];*)
If[Not[MatchQ[Flatten[soln],{(_->_)..}]],Throw[{"NSolve Failed in >fpForInitState for",{#//InputForm,theSys//InputForm,fpTarget,Stack[]}}],soln]]]&,initGuess,SameTest->mySameQ]






(*
hmatSymb=equationsToMatrix[
ccybEqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx}]//FullSimplify;
{zfSymb,hfSymb}=symbolicAR[hmatSymb];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{5}]]];
{bmatSymb,phimatSymb,fmatSymb}=symbolicComputeBPhiF[hmatSymb,qmatSymb]//FullSimplify;
Protect[hmat,qmat,bmat,phimat,fmat,psiz,psieps,psic,
hmatSymb,qmatSymb,bmatSymb,phimatSymb,fmatSymb,
zfSymb,hfSymb,amatSymb,evlsSymb,evcsSymb];
*)














(*need to factor out model below*)
(*

aPath01=genPath[{{mu1tm1},{mu2tm1},{mu3tm1},{kktm1},{vvtm1}},bmat,phimat,fmat,psieps,1]//numIt
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
theExp=(Expectation[z01ExactFunc[qtm1,rutm1,eps],eps \[Distributed] NormalDistribution[0,sigma$u/.ccybSubs]])//FullSimplify;
]
z01ExactExpFunc=Function @@ 
{{qtm1,rutm1},theExp//FullSimplify}

z01ExactInitRE={
Function[{xx,yy},z01ExactExpFunc[xx,yy][[1]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[2]]],
Function[{xx,yy},z01ExactExpFunc[xx,yy][[3]]]}




(*always used with epsVal=0*)
noCnstrnGuess= With[{linPFSys=
Flatten[bmat . {{qVal},{0},{ruVal}}+phimat . (psieps *0+psic)]//.ccybSubs},
{Function @@ {{qVal,ruVal},linPFSys[[1]]},Function @@ {{qVal,ruVal},linPFSys[[3]]}}]

Print["defining constraint and selector funcs"]
compCon={Function[{aPath,theZs},
(((aPath[[5,1]])>=(0.02(*rUnderBar//.ccybSubs*))&&theZs[[-1]]==0)||
((aPath[[5,1]])==(0.02(*rUnderBar//.ccybSubs*))&&theZs[[-1]]>=0))]}


stateSel=Function[aVec,{aVec[[1]],aVec[[3]]}]
stateSel={1,3};

getCcybFixedPoint[fpTarget_List,theSys_Function,initGuess_List]:=
FixedPoint[fpTarget/.With[{soln=
Flatten[NSolve[(theSys @@ #),fpTarget]]},(*Print["soln=",{soln,fpTarget,#,theSys @@ #,theSys[[2]],theSys}];*)
If[Not[MatchQ[soln,{(_->_)..}]],Throw[{"NSolve Failed in >fpForInitState for",{theSys//InputForm,fpTarget,Stack[]}}],soln]]&,initGuess,SameTest->mySameQ]

*)

Protect[getCcybFixedPoint,compCon,stateSel,ccybEqns,ccybSubs]
End[]

EndPackage[]
Print["done reading ccybSimpleModel.m"]
 
