PrependTo[$Path,"../../../paperProduction/mathAMA/AMAModel/"];
PrependTo[$Path,"../../../mathAMA/NumericAMA"];
PrependTo[$Path,"../../../mathAMA/SymbolicAMA"];
BeginPackage["labDocPrep`",{"ProjectionInterface`","JLink`","AMAModel`","NumericAMA`"}]

numericLinearizeSystemForOBC::usage="numericLinearizeSystemForOBC[eqns_List]"
nonFPart::usage="nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ]"
redoFPart::usage="redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,horizon_Integer,numCon_Integer]"


Begin["Private`"]
nonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,psimat_?MatrixQ]:=
bmat . xtm1 + phimat . psimat . epsilon

redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer]:=
redoFPart[phimat,fmat,psiz,horizon,numCon,0]


redoFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer]:=
With[{zMats=redoGenZVars[horizon,numCon,offset]},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]


numericLinearizeSystemForOBC[eqns_List]:=
Module[{noCnstr=eqns/.{Global`eps[_][_]->0,Global`eqvdIf[_,xx_,_]->xx},zf,hr,
bmat,phimat,fmat},Print[noCnstr];
With[{hmat=equationsToMatrix[noCnstr]},Print[hmat];
{ig,ig,ig,ig,qmat,ig,ig,ig}=numericAMA[hmat,1,1];Print[zf,hf];
Print["need to generalize to actually compute qmat"];
{hmat,qmat,{bmat,phimat,fmat}=numericComputeBPhiF[hmat,qmat,1]}
]]



(*isWindowsQ[]:=$OperatingSystem=="Windows"
If[isWindowsQ[],
$MAVENREPO="R:/Software/mavenRepositories/tryRep";
AddToClassPath[$MAVENREPO<>"/org/apache/commons/commons-math/2.0/commons-math-2.0.jar"];
AddToClassPath[$MAVENREPO<>"/ooNumMethods/OONumericalMethods/1.0/OONumericalMethods-1.0.jar"];
AddToClassPath[$JamaJar=$MAVENREPO<>"/math/nist/gov/Jama/1.0.3/Jama-1.0.3.jar"];
AddToClassPath[$ProjJar=$MAVENREPO<>"/gov/frb/ma/msu/ProjectionMethodToolsJava/0.0.1-SNAPSHOT/ProjectionMethodToolsJava-0.0.1-SNAPSHOT.jar"],
$MAVENREPO="/msu/res1/Software/mavenRepositories/tryRep/";
AddToClassPath[$MAVENREPO<>"/ooNumMethods/OONumericalMethods/1.0/OONumericalMethods-1.0.jar"];
AddToClassPath[$MAVENREPO<>"/org/apache/commons/commons-math/2.0/commons-math-2.0.jar"];
AddToClassPath[$JamaJar=$MAVENREPO<>"/math/nist/gov/Jama/1.0.3/Jama-1.0.3.jar"];
AddToClassPath[$ProjJar=$MAVENREPO<>"/gov/frb/ma/msu/ProjectionMethodToolsJava/0.0.1-SNAPSHOT/ProjectionMethodToolsJava-0.0.1-SNAPSHOT.jar"];
]
AddToClassPath[Directory[]];




(*

Get["prepPackages.mth"]; Get["applyReliable.mth"];
<<JavaGraphics`

lucaSubs = {betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
  rUnderBar -> 2/100, qLow -> -1/2, qHigh -> 1/2, 
  ruLow -> -4*sigma$u/(1 - rho$ru), ruHigh ->  4*sigma$u/(1 - rho$ru),
   integOrder -> {50}, sigma$u ->2/100, theMean -> {0}, rho$ru -> 1/2,
   adj -> 1}; 


mySubs={betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
  (*rUnderBar -> rub,*) qLow -> -1/2, qHigh -> 1/2, 
  ruLow -> -4*sigma$u/(1 - rho$ru), ruHigh ->  4*sigma$u/(1 - rho$ru),
   integOrder -> {50}, sigma$u ->2/100, theMean -> {0}, rho$ru -> 1/2,
   adj -> 1}




myN[xx_]:=(N[xx/.{t-1->$tm1,t+1->$tp1}])/.{$tm1->t-1,$tp1->t+1}

numIt[xx_]:=xx//.lucaSubs//myN//Expand//Chop

forFzComp[xtm1_?MatrixQ,epst_?MatrixQ,zt_?MatrixQ,ztp1Exp_?MatrixQ]:=(bmat .xtm1 + 
        phimat .(psieps . epst +
           psiz.zt) + 
        fmat .phimat . 
         psiz . ztp1Exp)//numIt


fzComp[xt_?MatrixQ,xtm1_?MatrixQ,epst_?MatrixQ,zt_?MatrixQ,ztp1Exp_?MatrixQ]:=
With[{theComp=forFzComp[xtm1,epst,zt,ztp1Exp]},
With[{zZap=Solve[myN[((theComp[[2,1]]//.lucaSubs))] == rUnderBar,zt[[1,1]]]//Flatten},Print[zZap//InputForm];
Join[Thread[
Flatten[xt-theComp] ==0],
{(discrep[t] - (theComp[[2,1]] - rUnderBar)/.zt[[1,1]]->0) == 0,
zt[[1,1]]-eqvdIf[discrep[t]>=0,0,
zt[[1,1]]/.zZap]==0}]]]


fzIterComp[
xt_?MatrixQ,xtm1_?MatrixQ,epst_?MatrixQ,zt_?MatrixQ,ztp1Exp_?MatrixQ]:=
With[{theComp=forFzComp[xtm1,epst,zt,ztp1Exp]},
With[{zZap=Solve[myN[
{theComp[[1,1]]==xt[[1,1]],theComp[[3,1]]==xt[[3,1]],myN[((theComp[[2,1]]))]== rUnderBar}//.
lucaSubs] ,{xt[[1,1]],xt[[3,1]],zt[[1,1]]}]//Flatten},Print[zZap//InputForm];
{zZap,Thread[
Flatten[xt-theComp] ==0]}]]



doIter[{zsubNow_List,eqnsNow_List}]:=
fzIterComp[
Transpose[{{qt,rt,rut}}],Transpose[{{qtm1,rtm1,rutm1}}],
{{eps}},{{zz}},{{zz/.zsubNow/.{eps->0,qtm1->qt,rtm1->rt,rutm1->rut}}}]//.lucaSubs//myN//Expand//Chop




forSimp = Union[Cases[bmat, Sqrt[___], Infinity]][[1]];
latexSubs = Join[N[lucaSubs],{(*forSimp -> \[Nu],*) 
betap -> Subscript[\[Beta], p], 
   phip -> Subscript[\[Phi], p], rhop -> Subscript[\[Rho], p], 
   rho$ru -> Subscript[\[Rho], ru], sigmap -> Subscript[\[Sigma], p],
xx_[yy:t+_.]->Subscript[xx,yy],zt0->Subscript[z,0],
uu$Shock->Subscript[\[Epsilon],t],
Subscript[discrep,t]->\[Delta],eqvdIf->\[CurlyTheta],
Subscript[ru,y:t+_.]->Subscript[Subscript[r,u],y]}];
Export["prettyHmat.pdf", MatrixForm[hmat //. latexSubs]];
Export["prettyBmat.pdf", MatrixForm[bmat //. latexSubs]];
Export["prettyPhimat.pdf", MatrixForm[phimat //. latexSubs]];
Export["prettyFmat.pdf", MatrixForm[fmat //. latexSubs]];
Export["prettyHmatCons.pdf", MatrixForm[hmatCons //. latexSubs]];
Export["prettyBmatCons.pdf", MatrixForm[bmatCons //. latexSubs]];
Export["prettyPhimatCons.pdf", MatrixForm[phimatCons //. latexSubs]];
Export["prettyFmatCons.pdf", MatrixForm[fmatCons //. latexSubs]];

applyBmat[xtm1_?MatrixQ,bmat_?MatrixQ,num_?NumberQ]:=
ArrayFlatten[Transpose[{NestList[bmat . #&,xtm1,num]}]]

applyFormula[xtm1_?MatrixQ,epst_?MatrixQ,zt_?MatrixQ,ztp1_?MatrixQ]:=
With[{xt=forFzComp[xtm1,epst,zt,ztp1]},xt]

compZ0[xtm1_?MatrixQ,epst_?MatrixQ,zt_?MatrixQ,ztp1_?MatrixQ]:=
With[{theComp=applyFormula[xtm1,epst,zt,ztp1]},
With[{zZap=(Solve[myN[((theComp[[2,1]]//.lucaSubs))] == rUnderBar,zt[[1,1]]]//Flatten)},{zZap,theComp/.zZap}]]






Get["thirdRecursion.mth"];

hmatN=hmat//.lucaSubs//myN;

morePaperSubs={
qtm1->Subscript[q,t-1],
rutm1->Subscript[ru,t-1],
rtm1->Subscript[rr,t-1],
eps->Subscript[\[Epsilon],t]
}

redExport[fName_String,gObj_Graphics3D]:=Export[fName,gObj,"AllowRasterization" -> True,ImageSize -> 360, ImageResolution -> 600,PlotRange->All]





doEpsValPlot[varIndx_Integer,epsIndx_Integer,theFuncs_List]:=
Show[
ListPointPlot3D[doAnEps[ptsSByEpsilon[[epsIndx]],varIndx],
PlotStyle->{PointSize[Large]},DisplayFunction->$DisplayFunction],
Plot3D[theFuncs[xx,yy,ptsSByEpsilon[[epsIndx,1,3]]][[varIndx]],{xx,-.5,.5},{yy,-.15,.15}],
PlotRange->All]

doAnEps[theVals:{
{_?NumberQ,_?NumberQ,xx_?NumberQ}..},theFuncs_List,pick_Integer]:=
{#[[1]],#[[2]],(theFuncs @@ #)[[pick]]}& /@ theVals
lucaEqnsCons=ReplacePart[lucaEqns,3->
rr[t] - Global`eqvdIf[phip*qq[t] >= rUnderBar,rUnderBar, phip*qq[t]]]

*)
*)
End[]
EndPackage[]

Print["done reading labDocPrep package context"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]
Global`ridUndef[xx_List]:=DeleteCases[xx,_->Undefined]
redExport[fName_String,gObj_Graphics3D]:=Export[fName,gObj,"AllowRasterization" -> True,ImageSize -> 360, ImageResolution -> 600,PlotRange->All]



Print["defining luca model equations along with hmat, qmat, bmat, phimat and fmat"]


lucaSubs = {betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
   rUnderBar -> 2/100, qLow -> -.5, qHigh -> .5, 
   ruLow -> -4*sigma$u/(1 - rho$ru), 
   ruHigh ->  4*sigma$u/(1 - rho$ru), integOrder -> {20}, 
   sigma$u -> 0.01, theMean -> {0}, rho$ru -> 0.5, adj -> 1};

   (*modParams = {betap, phip, rhop, rho$ru, sigmap} //. lucaSubs // N;*)
   modParams = {rUnderBar} //. lucaSubs // N;
lucaEqns = {qq[t] - (betap*(1 - rhop)*qq[t + 1] + rhop*qq[t - 1] - 
      sigmap*rr[t] + ru[t]),
   ru[t] - rho$ru*ru[t - 1] - adj*eps[uu][t],
   rr[t] - Global`eqvdIf[phip*qq[t] >= rUnderBar, phip*qq[t], rUnderBar]}


latexSubs = Join[N[lucaSubs],{(*forSimp -> \[Nu],*) 
betap -> Subscript[\[Beta], p], 
   phip -> Subscript[\[Phi], p], rhop -> Subscript[\[Rho], p], 
   rho$ru -> Subscript[\[Rho], ru], sigmap -> Subscript[\[Sigma], p],
xx_[yy:t+_.]->Subscript[xx,yy],zt0->Subscript[z,0],
uu$Shock->Subscript[\[Epsilon],t],
Subscript[discrep,t]->\[Delta],Global`eqvdIf->\[CurlyTheta],
Subscript[ru,y:t+_.]->Subscript[Subscript[r,u],y]}];



morePaperSubs={
qtm1->Subscript[q,t-1],
rutm1->Subscript[ru,t-1],
rtm1->Subscript[rr,t-1],
eps->Subscript[\[Epsilon],t]
}




{hmat,qmat,{bmat,phimat,fmat}}=numericLinearizeSystemForOBC[(lucaEqns//.(lucaSubs)//Rationalize[#,1/100000000]&)]
psiz={{0},{0},{1}};
psieps={{0},{1},{0}};

Export["prettyHmat.pdf", MatrixForm[hmat //. latexSubs//N]];
Export["prettyBmat.pdf", MatrixForm[bmat //. latexSubs//N]];
Export["prettyPhimat.pdf", MatrixForm[phimat //. latexSubs//N]];
Export["prettyFmat.pdf", MatrixForm[fmat //. latexSubs//N]];

Global`$MaxSolveTime=900;


Print["interp defs"]
Global`makeInterpFunc[theFunc_Function]:=
FunctionInterpolation @@ {theFunc[Global`qq,Global`ru,Global`ep],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,
{Global`ep,-2*Global`sigma$u,2*Global`sigma$u}//.Global`lucaSubs//N(*,
InterpolationOrder -> 10, InterpolationPrecision -> 30, 
 AccuracyGoal -> 15, PrecisionGoal -> 15, 
 InterpolationPoints -> 100, MaxRecursion -> 25*)}


Global`makeInterpFunc[theFunc_Function,iOrder_Integer,iPts_Integer]:=
FunctionInterpolation @@ {theFunc[Global`qq,Global`ru,Global`ep],
{Global`qq,Global`qLow,Global`qHigh}//.Global`lucaSubs//N,
{Global`ru,Global`ruLow,Global`ruHigh}//.Global`lucaSubs//N,
{Global`ep,-2*Global`sigma$u,2*Global`sigma$u}//.Global`lucaSubs//N,
InterpolationOrder ->iOrder, 
InterpolationPoints -> iPts(*, InterpolationPrecision -> 30, 
 AccuracyGoal -> 15, PrecisionGoal -> 15, MaxRecursion -> 25*)}
Print["interp defs"]


Print["interp defs"]


Global`timeMakeInterpFunc[theFunc_Function]:=
Timing[Global`makeInterpFunc[theFunc]]



Global`timeMakeInterpFunc[theFunc_Function,iOrder_Integer,iPts_Integer]:=
Timing[Global`timeMakeInterpFunc[theFunc,iOrder,iPts]]
Print["interp defs"]


Global`infNorm[func_]:=
NMaximize @@ 
{{func[Global`qq,Global`ru,Global`ep],
(((Global`qLow<=Global`qq<=Global`qHigh)//.Global`lucaSubs)//N)&&
(((Global`ruLow<=Global`ru<=Global`ruHigh)//.Global`lucaSubs)//N)&&
(((-2*Global`sigma$u<=Global`ep<=2*Global`sigma$u)//.Global`lucaSubs)//N)
},
{Global`qq,Global`ru,Global`ep},Method->{"RandomSearch","SearchPoints"->50}}
Print["interp defs"]


Global`experOrd[]:=
Module[{timeInterp=Flatten[
Table[Join[Global`timeMakeInterpFunc[Global`zzz$0$1Func,io,ip],{io,ip}],
{io,1,4},{ip,10,100,10}],1]},
{#[[1]],Global`infNorm[cmpExct[#[[2]]]],#[[3]],#[[4]],#[[2]]}&/@ timeInterp
]

Print["interp defs"]

Global`cmpExct[aFunc_]:=
Function[{Global`qq,Global`ru,Global`ep},Abs[aFunc[Global`qq,Global`ru,Global`ep]-Global`zzz$0$1Func[Global`qq,Global`ru,Global`ep]]]



Print["done defs"]
