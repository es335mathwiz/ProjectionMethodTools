Print["reading simpleRBCModel.m"]
BeginPackage["simpleRBCModel`",{"occBindRecur`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","ProjectionInterface`"}]

cc::usage="rbc model variable"
kk::usage="rbc model variable"
alpha::usage="rbc model parameter"
delta::usage="rbc model parameter"
compCon::usage="compCon[aPath_?MatrixQ]:=Function[{aPath,theZs}"
stateSel::usage="stateSel[aPath_?MatrixQ]:=Function[{aPath}"
rbcEqns::usage="rbc model equations"
Begin["Private`"]

(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
cobb douglas production*)

rbcEqns={
 1/cc[t]-(delta*((E^eps[theta][t])*(1/cc[t+1])*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((E^eps[theta][t])*(kk[t-1]^alpha))
}
(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100
}





rbcSimp=(rbcEqns)/.paramSubs//myN;

rbcSSEqns=Thread[(rbcSimp/.{eps[_][_]->0,xx_[t+_.]->xx})==0]//myN


ssSoln=FindRoot[rbcSSEqns,{{cc,1},{kk,1}}]

psieps=Transpose[{((D[#,eps[Private`theta][t]]&/@ Private`rbcSimp)/.{eps[_][_]->0,xx_[t+_.]->xx})/.ssSoln}]

psic={{cc},{kk}}/.ssSoln
psiz=IdentityMatrix[2]

hmatSymbPre=equationsToMatrix[
rbcSimp/.{eps[_][_]->0}]//FullSimplify;
hmat=(hmatSymbPre/.xx_[t+_.]->xx)/.ssSoln;
{ig, ig, ig, ig, qmat, ig, ig, ig} = NumericAMA`numericAMA[hmat, 1, 1]; 
{bmat, phimat, fmat} = NumericAMA`numericComputeBPhiF[hmat,qmat] 

Print["defining constraint and selector funcs"]
compCon={
Function[{aPath,theZs},0==rbcSimp[[1]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[4,1]],
cc[t]->aPath[[3,1]],cc[t+1]->aPath[[5,1]],eps[theta][t]->eps}],
Function[{aPath,theZs},0==rbcSimp[[2]]/.{
kk[t-1]->aPath[[2,1]],kk[t]->aPath[[4,1]],
cc[t]->aPath[[3,1]],cc[t+1]->aPath[[5,1]],eps[theta][t]->eps}]}

stateSel={2}


End[]
EndPackage[]
Print["done reading simpleRBCModel.m"]
