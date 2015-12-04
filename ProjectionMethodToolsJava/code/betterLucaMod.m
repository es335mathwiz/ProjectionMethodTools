PrependTo[$Path,"../../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading betterLucaMod.m"]
BeginPackage["betterLucaMod`",{"AMASeriesRepresentation`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"}]

		
discreteDist::usage="charactizes the distribution of epsilon"
lucaDist::usage="charactizes the distribution of epsilon"
lucaLinMod::usage="linear model matrices for approx"
lucaSubs::usage="luca model param subs"
lucaEqnFuncs::usage="makeFunc[allFuncs]"
lucaX0Z0::"genX0Z0Funcs[lucaLinMod]";
Begin["Private`"]


lucaSubs = {betap -> 1/100(*99/100*), phip -> 1, rhop -> 1/2, sigmap -> 1, 
   rUnderBar -> 2/100, qLow -> -.5, qHigh -> .5, 
   ruLow -> -4*sigma$u/(1 - rho$ru), 
   ruHigh ->  4*sigma$u/(1 - rho$ru), integOrder -> {20}, 
   sigma$u -> 0.01, theMean -> {0}, rho$ru -> 0.5, adj -> 1,rstar->3/100};

   (*modParams = {betap, phip, rhop, rho$ru, sigmap} //. lucaSubs // N;*)
   modParams = {rUnderBar} //. lucaSubs // N;



lucaEqns = {qq[t] - (betap*(1 - rhop)*qq[t + 1] + rhop*qq[t - 1] - 
      sigmap*rr[t] + ru[t]),
   (ru[t]-rstar) - rho$ru*(ru[t - 1]-rstar) - adj*eps[uu][t],
   rr[t] - eqvdIf[phip*qq[t] >= rUnderBar, phip*qq[t], rUnderBar]};




{hmat,qmat,{bmat,phimat,fmat}}=(numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)]//myN);
psic=({{0},{(1-rho$ru)*rstar},{0}}//.lucaSubs)//myN;
(*psiz={{0},{0},{1}};IdentityMatrix[3];*)
psiz=IdentityMatrix[3];
psieps={{0},{1},{0}};


hmatSymb=equationsToMatrix[
lucaEqns/.{eps[_][_]->0,eqvdIf[_,xx_,_]->xx}]//FullSimplify;
{zfSymb,hfSymb}=symbolicAR[hmatSymb];
amatSymb=symbolicTransitionMatrix[hfSymb];
{evlsSymb,evcsSymb}=Eigensystem[Transpose[amatSymb]];
qmatSymb=Join[zfSymb,evcsSymb[[{5}]]];
{bmatSymb,phimatSymb,fmatSymb}=symbolicComputeBPhiF[hmatSymb,qmatSymb]//FullSimplify;


lucaLinMod={hmatSymb//N,bmatSymb // N, phimatSymb // N, 
    fmatSymb // N, psieps // N, 
    psic // N, psiz // N,{{0}}}//.lucaSubs;

lucaX0Z0=genX0Z0Funcs[lucaLinMod];



lucaDist={{{ee,NormalDistribution[0,sigma$u]}}}//.lucaSubs;
discreteDist={{{bb,BernoulliDistribution[0.2]}}}//.lucaSubs;


lucaGuts=Private`lucaEqns//.lucaSubs//InputForm

eqnSubs=
{
qq[t-1]->qqtm1,rr[t-1]->rrtm1,ru[t-1]->rutm1,
qq[t]->qqt,rr[t]->rrt,ru[t]->rut,
qq[t+1]->qqtp1,rr[t+1]->rrtp1,ru[t+1]->rutp1,
eps[uu][t]->epsVal
};


compArgs=
{
{qqtm1,_Real},{rrtm1,_Real},{rutm1,_Real},
{qqt,_Real},{rrt,_Real},{rut,_Real},
{qqtp1,_Real},{rrtp1,_Real},{rutp1,_Real},
{epsVal,_Real}
};
funcArgs=
{qqtm1,rrtm1,rutm1,
qqt,rrt,rut,
qqtp1,rrtp1,rutp1,
epsVal};






theCond=lucaGuts[[1,3]]/.{vv_-eqvdIf[xx_,yy_,zz_]->{vv,xx,yy,zz}}

theLilHDet0=({lucaGuts[[1,1]],
lucaGuts[[1,2]],
theCond[[1]]-theCond[[3]]}/.{_[t+1]->0})/.eqnSubs;
lilHDet0Func= Function @@ {funcArgs,theLilHDet0};

theLilHDet1=({lucaGuts[[1,1]],
lucaGuts[[1,2]],
theCond[[1]]-theCond[[4]]}/.{_[t+1]->0})/.eqnSubs;
lilHDet1Func= Function @@ {funcArgs,theLilHDet1};

theBigHDet={{Coefficient[lucaGuts[[1,1]],Private`qq[t+1]],0,0},{0,0,0},{0,0,0}}
bigHDetFunc= Function @@ {funcArgs,theBigHDet};



cndtn=theCond[[2]]/.qq[t]->qqt;
theSelFunc= Function @@{funcArgs,
Switch[cndtn,True,0,False,1]}

allFuncs={theSelFunc,{{lilHDet0Func,bigHDetFunc},{lilHDet1Func,bigHDetFunc}}}



lucaEqnFuncs=makeFunc[funcArgs,3,allFuncs]

End[]
EndPackage[]




