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
bmRSwitchProbFunc::usage="bmRSwitchProbFunc = Function @@{Append[funcArgs[[Range[4]]],epsVal]}"


Begin["Private`"]






bmRSwitchEqns={
$1ii[t] -($1thePi[t+1] + $1rr[t]),
$1rr[t]= rho * $1rr[t-1] +eps[u][t],
$1ii[t]= $1ap1ha * $1thePi[t],
$2ii[t] -($2thePi[t+1] + $2rr[t]),
$2rr[t]= rho * $2rr[t-1] +eps[u][t],
$2ii[t]=$2alpha * $2thePi[t]
}
(*but see page 18*)
bmRSwitchParamSubs={$1ap1ha->.9, 
$2alpha->3,rho->.99,lam->.1,probGamma->1,
$1gamma->.3,$2gamma->.2}



funcArgs=
{
$1iitm1,
$1rrtm1,
$1thePitm1,
$1regimetm1,
$2iitm1,
$2rrtm1,
$2thePitm1,
$2regimetm1,
$1iit,
$1rrt,
$1thePit,
$1regimet,
$2iit,
$2rrt,
$2thePit,
$2regimet,
$1iitp1,
$1rrtp1,
$1thePitp1,
$1regimetp1,
$2iitp1,
$2rrtp1,
$2thePitp1,
$2regimetp1,
epsVal
}

lilHDet0Guts={
$1iit - ($1thePitp1 + $1rrt),
$1rrt - ( rho * $1rrtm1 +epsVal),
$1iit - $1ap1ha * $1thePit,
$1regimet-1,
$2iit - ($2thePitp1 + $2rrt),
$2rrt - ( rho * $2rrtm1 +epsVal),
$2iit - ($2alpha * $2thePit),
$2regimet-2
}

lilHDet0Func= Function @@ {funcArgs,lilHDet0Guts}//.bmRSwitchParamSubs;


bigHDet0Guts={
{0,0,-1,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,-1,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0}
}


bigHDet0Func= Function @@ {funcArgs,bigHDet0Guts}//.bmRSwitchParamSubs;


(* {rho, $1ap1ha, $2alpha, the$1Pi[t], $1rr[-1 + t], $1thePi[1 + t], 
    $2rr[-1 + t], $2thePi[t], $2thePi[1 + t], eps[u][t]}*)
allFuncs={lilHDet0Func,bigHDet0Func}

bmRSwitchEqnFuncs=makeFunc[funcArgs,8,allFuncs]

pBarMat={
{.95, 0.05},
{0.2, 0.8}};
lambdaMat={
{0,0},
{-lam,lam}}
probGuts =pBarMat+probGamma*lambdaMat*($1thePitm1^2)

bmRSwitchProbFunc = Function @@{Append[funcArgs[[Range[4]]],epsVal],probGuts}//.bmRSwitchParamSubs;

(*

pp[ii_Integer,jj_Integer]:= ppBar[ii,jj] + gamma lambda[ii,jj]*$1thePi[t-1]^2
*)


End[]
EndPackage[]

