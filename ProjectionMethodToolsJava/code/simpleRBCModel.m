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
compCon::usage="compCon[aPath_?MatrixQ]:=Function[{aPath,theZs}"
rbcEqns::usage="rbc model equations"
condExp::usage="condExp[kktm1_?NumberQ,ii_Integer]"
fixCondExp::usage="condExp[kktm1_?NumberQ,ii_Integer]"
compZs::usage="compZs[hmatNum_?MatrixQ,kktm1_?NumberQ,ii_Integer]"

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


rbcEqnsExt={
  1/cc[t]-(delta*((ratioThetaToC[t+1])*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
(* Log[theta[t]]-rho*Log[theta[t-1]] - eps[theta][t],*)
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
 ratioThetaToC[t]-(theta[t]/cc[t])
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
tog=Join[paramSubs,forParamSubs]








If[Length[cSSSub]===0,
Print["computing steady state subs"];
rbcSSEqns=Thread[(rbcEqns//.{eps[_][_]->0,xx_[t+_.]->xx})==0];
kSSSub=PowerExpand[Simplify[Solve[delta*alpha*kk^alpha==kk,{kk},Reals],(0<alpha<1)&&(0<delta<1)][[2]]];
cSSSub=Flatten[Solve[Simplify[rbcSSEqns//.kSSSub][[2]],cc]];
ssSolnSubs=Join[cSSSub,kSSSub,{theta->1,ratioThetaToC->1/cc}];
rbcSSEqns=Thread[(rbcEqns//.{eps[_][_]->0,xx_[t+_.]->xx})==0];
Private`rbcSSThetaREEqn=Solve[(Log[theta]==Mean[LogNormalDistribution[0,sigma/.Private`paramSubs]])//.Private`tog,theta]
]
((alpha*delta)//.tog//N)*thNow*lastK^(alpha//.tog//N)


hmatSymbRaw=(((equationsToMatrix[
rbcEqns]//FullSimplify)/.{xx_[t+_.]->xx})//.ssSolnSubs)/.{eps[_]->0}//FullSimplify;
hmatSymbRawExt=(((equationsToMatrix[
rbcEqnsExt]//FullSimplify)/.{xx_[t+_.]->xx})//.ssSolnSubs)/.{eps[_]->0}//FullSimplify;
rbcSimp=(rbcEqns)//.tog
rbcSimpExt=(rbcEqnsExt)//.tog

psiepsSymb=-Transpose[{((D[#,eps[theta][t]]&/@ rbcSimp)/.{eps[_][_]->0,xx_[t+_.]->xx})//.ssSolnSubs}]
psieps=psiepsSymb//.tog;


psiepsSymbExt=-Transpose[{((D[#,eps[theta][t]]&/@ rbcSimpExt)/.{eps[_][_]->0,xx_[t+_.]->xx})//.ssSolnSubs}]
psiepsExt=psiepsSymbExt//.tog;



psiz=IdentityMatrix[3]
psizExt=IdentityMatrix[4]



hmatSymb=hmatSymbRaw//.simpSubs
hSum=hmatSymb[[All,Range[3]]]+hmatSymb[[All,3+Range[3]]]+hmatSymb[[All,6+Range[3]]];
ssSolnVec={{cc},{kk},{theta}}//.ssSolnSubs;
psicSymb=hSum . ssSolnVec;
psic=psicSymb//.tog


hmatSymbExt=hmatSymbRawExt//.simpSubs
hSumExt=hmatSymbExt[[All,Range[4]]]+hmatSymbExt[[All,4+Range[4]]]+hmatSymbExt[[All,8+Range[4]]];
ssSolnVecExt={{cc},{kk},{ratioThetaToC},{theta}}//.ssSolnSubs;
psicSymbExt=hSumExt . ssSolnVecExt;
psicExt=psicSymbExt//.tog


{zfSymb,hfSymb}=symbolicAR[hmatSymb//.tog];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{1}]]];



		      
{zfSymbExt,hfSymbExt}=symbolicAR[hmatSymbExt//.tog];
amatSymbExt=symbolicTransitionMatrix[hfSymbExt];
{evlsSymbExt,evcsSymbExt}=Eigensystem[Transpose[amatSymbExt]];
qmatSymbExt=Join[zfSymbExt,evcsSymbExt[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]


hmat=hmatSymb//.tog;
qmat=qmatSymb//.tog;
Print["computing and simplifying the symbolic b phi f etc"]
{bmat,phimat,fmat}=symbolicComputeBPhiF[hmat,qmat]//Simplify;


hmatExt=hmatSymbExt//.tog;
qmatExt=qmatSymbExt//.tog;
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

rhoVal=rho//.tog//N;

condExp=
Compile[{{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},
{ii,_Integer}},
With[{thVals=Join[{},Drop[NestList[E^(rhoVal*Log[#])&,E^(rhoVal*Log[thtm1]+epsVal),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=Drop[yyVals,0]-Drop[kkVals,1]},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]]


fixCondExp[cctm1_,kktm1_,thtm1_,epsVal_,ii_]:=
With[{thVals=Join[{},Drop[NestList[E^(rhoVal*Log[#])&,E^(rhoVal*Log[thtm1]+epsVal),ii],-1]]},
With[{kkVals=Drop[FoldList[nxtK,kktm1,thVals],0]},
With[{yyVals=MapThread[yNow,{Drop[kkVals,-1],Drop[thVals,0]}]},
With[{ccVals=Drop[yyVals,0]-Drop[kkVals,1]},
With[{thetransp=Partition[Flatten[Transpose[{Flatten[ccVals],Flatten[Drop[kkVals,1]],Flatten[Drop[thVals,0]]}]],1]},
Join[{{cctm1},{kktm1},{thtm1}},thetransp]]]]]]



nxtK[lastK_,thNow_]:=((alpha*delta)//.tog//N)*thNow*lastK^(alpha//.tog//N)

yNow[kLag_,thNow_]:=thNow*kLag^(alpha//.tog//N)

compZs=Compile[{{tryMat,_Real,2},{phimat,_Real,2},{psieps,_Real,2},{psic,_Real,2},{cctm1,_Real},{kktm1,_Real},{thtm1,_Real},{epsVal,_Real},{ii,_Integer}},
With[{thePath=Flatten[condExp[cctm1,kktm1,thtm1,epsVal,ii+2]]},
With[{raw=Flatten[
Table[(tryMat . Transpose[{thePath[[jj*3+Range[9]]]}])-psic,{jj,0,ii-1}]]},
With[{epsPart=PadRight[Flatten[-(psieps *epsVal)],Length[raw]]},
raw+epsPart
]]]]






rbcEqnsApply[{ctm1_,ktm1_,ct_,kt_,ctp1_,ktp1_}]:=
rbcEqns/.eps[theta][t]->0/.{cc[t]->ct,cc[t+1]->ctp1,kk[t-1]->ktm1,kk[t]->kt}//.tog



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



End[]
EndPackage[]
Print["done reading simpleRBCModel.m"]

