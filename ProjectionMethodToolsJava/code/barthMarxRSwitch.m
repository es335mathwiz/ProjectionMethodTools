PrependTo[$Path,"../../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading barthMarxRSwitch.m"]
BeginPackage["barthMarxRSwitch`",{"AMASeriesRepresentation`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"}]
(*regime[t] like epsilon in timing, but "history matters"*)

bmRSwitchEqnFuncs::usage="makeFunc[funcArgs,8,allFuncs]bmRSwitchEqnFuncs=makeFunc[funcArgs,8,allFuncs]"
bmRSwitchProbFunc::usage="bmRSwitchProbFunc = Function @@{Append[funcArgs[[Range[3]]],epsVal]}"


Begin["Private`"]





  (*
bmRSwitchEqns={
$1ii[t] -($1thePi[t+1] + $1rr[t]),
$1rr[t]= rho * $1rr[t-1] +eps[u][t],
$1ii[t]= $1alpha * $1thePi[t],
$2ii[t] -($2thePi[t+1] + $2rr[t]),
$2rr[t]= rho * $2rr[t-1] +eps[u][t],
$2ii[t]=$2alpha * $2thePi[t]
  }*)
(*but see page 18*)
bmRSwitchParamSubs={$0alpha->.9, 
$1alpha->3,rho->.99,lam->.1,probGamma->1,
$1gamma->.3,$2gamma->.2}



funcArgs=
{
iitm1,
rrtm1,
thePitm1,
iit,
rrt,
thePit,
iitp1,
rrtp1,
thePitp1,
epsVal,
regimeVal
}

lilHDet0Guts={
iit - (thePitp1 + rrt),
rrt - ( rho * rrtm1 +epsVal),
iit - $0alpha * thePit
}
lilHDet1Guts={
iit - (thePitp1 + rrt),
rrt - ( rho * rrtm1 +epsVal),
iit - $1alpha * thePit
}

lilHDet0Func= Function @@ {funcArgs,lilHDet0Guts}//.bmRSwitchParamSubs;
lilHDet1Func= Function @@ {funcArgs,lilHDet1Guts}//.bmRSwitchParamSubs;


bigHDetGuts={
{0,0,-1},
{0,0,0},
{0,0,0}
}


bigHDetFunc= Function @@ {funcArgs,bigHDetGuts}//.bmRSwitchParamSubs;


(* {rho, $1ap1ha, $2alpha, the$1Pi[t], $1rr[-1 + t], $1thePi[1 + t], 
    $2rr[-1 + t], $2thePi[t], $2thePi[1 + t], eps[u][t]}*)


theSelFunc= Function @@{funcArgs,
Switch[regimeVal,0,0,1,1]}
allFuncs=
  {theSelFunc,{{lilHDet0Func,bigHDetFunc},{lilHDet1Func,bigHDetFunc}}}

bmRSwitchEqnFuncs=makeFunc[funcArgs,3,allFuncs]

pBarMat={
{.95, 0.05},
{0.2, 0.8}};
lambdaMat={
{0,0},
{-lam,lam}}
probGuts =pBarMat+probGamma*lambdaMat*($1thePitm1^2)

bmRSwitchProbFunc = Function @@
{Append[funcArgs[[Range[3]]],epsVal],probGuts}//.bmRSwitchParamSubs;

(*

pp[ii_Integer,jj_Integer]:= ppBar[ii,jj] + gamma lambda[ii,jj]*$1thePi[t-1]^2
*)


End[]
EndPackage[]
Print["done reading barthMarxRSwitch.m"]

