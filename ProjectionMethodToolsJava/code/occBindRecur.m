PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
BeginPackage["occBindRecur`",{"ProtectedSymbols`","ProjectionInterface`"(*,"JLink`","AMAModel`","NumericAMA`"*)}]
genCompSlack::usage="genCompSlack[pathLen_Integer]"

numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"
lucaEqns::usage="simple model equations"
lucaSubs::usage="typical simple model parameters"

qq::usage="variable for model"
rr::usage="variable for model"
ru::usage="variable for model"
rhop::usage="parameter for model"
betap::usage="parameter for model"
sigmap::usage="parameter for model"
rho$ru::usage="parameter for model"
adj::usage="parameter for model"
uu::usage="parameter for model"
phip::usage="parameter for model"
rUnderBar::usage="parameter for model"
Protect[qq,rr,ru]


Print["need to fix global t and eps, give them a protected role in some namespace probably the 'protectedSymbolNames' package"]





Begin["Private`"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]


 
 
(*

aPath03ValsRec=genPath[03];



qrAccTry03ValsRec[Global`qqTry_?NumberQ,Global`ruTry_?NumberQ]:={
((aPath03ValsRec[[5,1]]>=0.02&&Global`zzz$2$1[t]==0)||
(aPath03ValsRec[[5,1]]==0.02&&Global`zzz$2$1[t]>=0))&&
Global`qq[t]==aPath03ValsRec[[4,1]]&&
Global`ru[t]==aPath03ValsRec[[6,1]]&&
Global`zzz$1$1[t]==
Global`zzz$1$1AccInterpFunc[Global`qqTry,Global`ruTry,0]&&
Global`zzz$0$1[Global`t]==
Global`zzz$0$1AccInterpFunc[Global`qqTry,Global`ruTry,0]
}
*)


genCompSlack[pathLen_Integer]:=
With[{aPath=genPath[pathLen],
theZs=Flatten[ProjectionInterface`Private`redoGenZVars[pathLen,1]]},
(*((aPath[[5,1]]>=0.02&&zzz$2$1[t]==0)||
(aPath[[5,1]]==0.02&&zzz$2$1[t]>=0))&&
qq[t]==aPath[[4,1]]&&
ru[t]==aPath[[6,1]]*)
{aPath,theZs}
]


numericLinearizeSystemForOBC[eqns_List]:=
Module[{noCnstr=eqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx},zf,hr,
bmat,phimat,fmat},Print[noCnstr];
With[{hmat=equationsToMatrix[noCnstr]},Print[hmat];
{ig,ig,ig,ig,qmat,ig,ig,ig}=numericAMA[hmat,1,1];Print[zf,hf];
Print["need to generalize to actually compute qmat"];
{hmat,qmat,{bmat,phimat,fmat}=numericComputeBPhiF[hmat,qmat]}
]]

Print["defining luca model equations along with hmat, qmat, bmat, phimat and fmat"]


(*
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
*)
(*



{hmat,qmat,{bmat,phimat,fmat}}=numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)]
psiz={{0},{0},{1}};
psieps={{0},{1},{0}};

*)

End[]
EndPackage[]
