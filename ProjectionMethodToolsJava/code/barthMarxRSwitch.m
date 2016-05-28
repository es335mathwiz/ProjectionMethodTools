PrependTo[$Path,"../../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading barthMarxRSwitch.m"]
BeginPackage["barthMarxRSwitch`",{"AMASeriesRepresentation`",
      "ProtectedSymbols`","SymbolicAMA`","NumericAMA`","AMAModel`"}]
(*regime[t] like epsilon in timing, but "history matters"*)

bmRSwitchDist::usage="bmRSwitchDist"
bmRSwitchDistPF::usage="bmRSwitchDist"
bmRSwitchX0Z0::usage="bmRSwitchX0Z0";
bmRSwitchParamSubs::usage="bmRSwitchParamSubs"
bmRSwitchLinMod::usage="bmRSwitchLinMod" 
bmRSwitchEqnFuncs::usage="makeRegimeFunc[funcArgs,8,allFuncs]bmRSwitchEqnFuncs=makeRegimeFunc[funcArgs,8,allFuncs]"
bmRSwitchProbFunc::usage="bmRSwitchProbFunc = Function @@{Append[funcArgs[[Range[3]]],epsVal]}"


Begin["Private`"]




(*but see page 18*)
bmRSwitchParamSubs={$0alpha->0.9, 
$1alpha->3,rho->.99,lam->.01,probGamma->1,
$1gamma->.3,$2gamma->.2,sigma$u->0.01}



funcArgs=
{
regimetm1,
iitm1,
rrtm1,
thePitm1,
regimet,
iit,
rrt,
thePit,
regimetp1,
iitp1,
rrtp1,
thePitp1,
epsVal,
regimeVal
}

lilHDet0Guts={
iit - ( rrt),
rrt - ( rho * rrtm1 +epsVal),
iit - $0alpha * thePit,
regimet(*-regimetm1*)
}
lilHDet1Guts={
iit - ( rrt),
rrt - ( rho * rrtm1 +epsVal),
iit - $1alpha * thePit,
regimet(*-regimetm1*)
}

lilHDet0Func= Function @@ {funcArgs,lilHDet0Guts}//.bmRSwitchParamSubs;
lilHDet1Func= Function @@ {funcArgs,lilHDet1Guts}//.bmRSwitchParamSubs;


bigHDetGuts={
{0,0,0,-1},
{0,0,0,0},
{0,0,0,0},
{0,0,0,0}
}


bigHDetFunc= Function @@ {funcArgs,bigHDetGuts}//.bmRSwitchParamSubs;


(* {rho, $1ap1ha, $2alpha, the$1Pi[t], $1rr[-1 + t], $1thePi[1 + t], 
    $2rr[-1 + t], $2thePi[t], $2thePi[1 + t], eps[u][t]}*)


theSelFunc= Function @@{funcArgs,
Switch[regimeVal,0,0,1,1]}
allFuncs=
  {theSelFunc,{{lilHDet0Func,bigHDetFunc},{lilHDet1Func,bigHDetFunc}}}

bmRSwitchEqnFuncs=makeRegimeFunc[funcArgs,4,allFuncs]

pBarMat={
{.95, 0.05},
{0.2, 0.8}};
lambdaMat={
{0,0},
{-lam,lam}}
probGuts =pBarMat+probGamma*lambdaMat*(thePitm1^2)


bmRSwitchProbFunc = 
ReplacePart[
Function[xxxxxxx,probGuts[[regimetm1+1,toRegime+1]]//.bmRSwitchParamSubs],
{1->Join[funcArgs[[Range[4]]],{epsVal,toRegime}]}]



bmRSwitchDist={{{ee,NormalDistribution[0,sigma$u]}},
{2,$transFuncNoShocks,bmRSwitchProbFunc}}//.bmRSwitchParamSubs;
bmRSwitchDistPF={{{ee,PerfectForesight}},
{2,$transFuncNoShocks,bmRSwitchProbFunc}}//.bmRSwitchParamSubs;

(*
eqnArgs=
{
ii[t-1],
regime[t-1],
rr[t-1],
thePi[t-1],
ii[t],
regime[t],
rr[t],
thePi[t],
ii[t+1],
regime[t+1],
rr[t+1],
thePi[t+1],
eps[u][t],
1
}



bmRSwitchEqns=
bmRSwitchEqnFuncs @@ eqnArgs


{hmat,qmat,{bmat,phimat,fmat}}=(numericLinearizeSystemForOBC[
(bmRSwitchEqns//.(bmRSwitchParamSubs)//Rationalize[#,1/100000000]&)]//myN);
psic=({{0},{0},{0},{0}}//.bmRSwitchParamSubs)//myN;

psiz=IdentityMatrix[4];
psieps={{0,0},{1,0},{0,0},{0,1}};
(*psieps={{0},{1},{0},{0}};*)


hmatSymb=equationsToMatrix[
bmRSwitchEqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx}]//FullSimplify;
{zfSymb,hfSymb}=symbolicAR[hmatSymb];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{1}]]];
{bmatSymb,phimatSymb,fmatSymb}=symbolicComputeBPhiF[hmatSymb,qmatSymb]//FullSimplify;


bmRSwitchLinMod={hmatSymb//N,bmatSymb // N, phimatSymb // N, 
    fmatSymb // N, psieps // N, 
    psic // N, psiz // N,{{0}}}//.bmRSwitchParamSubs;

bmRSwitchX0Z0=genX0Z0Funcs[bmRSwitchLinMod];



*)
End[]
EndPackage[]
Print["done reading barthMarxRSwitch.m"]

