(* Mathematica Package *)
(*ToDo make sure rationals mapped to numbers beofre java sees them otherwise obscure error MLGet out of sequence
*)
BeginPackage["ProjectionInterface`", {"ProtectedSymbols`", "JLink`"}]

Print["reading ProjectionInterface`"];
(* Exported symbols added here with SymbolName::usage *)  
(* Exported symbols added here with SymbolName::usage *) 
myN::usage="nyN[xx_] leaves t values untouched by N"
newGenSys::usage="newGenSys";
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer] genZVars[horizons,numConstr,offset]"

occBindCreatePolynomialsLeadsOK::usage="occBindCreatePolynomialsLeadsOK";
newDoRecurIneqOcc::usage="newDoRecurIneqOcc"
nxtWts::usage="    nxtWts[prevWts_?MatrixQ,oldMaxPows_List,newMaxPows_List]"
GetCnstrnsReplaceVariables::usage="GetCnstrnsReplaceVariables[theMod_,thePolys_List,{stateStr_List,nonStateStr_List},theLocs_?ArrayQ]"
(*genPath::usage="genPath[num_Integer]"*)
genValsPath::usage="genValsPath[num_Integer]"
genCompSlackSys::usage="genCompSlackSys[nn_Integer]"
genPolyFuncs::usage="genPolyFuncs[projresults]"
genProjComponents::usage="genProjComponents[numZs_Integer]"
(*
genPolysFromBasis::usage="genPolysFromBasis[theBasis_?JavaObjectQ,theWts_?MatrixQ]"
*)
GetCnstrnsReplaceVariables::usage="GetCnstrnsReplaceVariables[theMod_,thePolys_List,{stateStr_List,nonStateStr_List},theLocs_?ArrayQ]"
tryNow::usage=""
occBindCreatePolynomials::usage="occBindCreatePolynomials"
CreateRHSPolynomials::usage="CreateRHSPolynomials[aMod_,results_?JavaObjectQ]"
Global`eqvdif::usage="eqvdif placeholder for EquationValDrv in substitutions";
EquationValDrv::usage="java class for projection";
doRecurIneqOcc::usage="doRecurIneqOcc[{}]";
doRecurIneqOccEqns::usage="doRecurIneqOcc[{}]";
genProjResults::usage="genProjResults";
fixDoRecurIneqOcc::usage="fixDoRecurIneqOcc";
gtChebNodes::usage="gtChebNodes[aWSB_?JavaObjectQ]"
gtXFormedChebSubsNotStrings::usage="gtXFormedChebSubsNotStrings"
getPhiFunc::usage="getPhiFunc[vName_String,theBasis_?JavaObjectQ]";
applyNew::usage="applyNew[phiNow_?NumberQ,theOrders_List,newModEqns_?JavaObjectQ]\n for AKY paper. 'homotopy' for computing phi"
GenerateBasis::usage="GenerateBasis[{stateVars_List,nonStateVars_List},
	initRanges_?MatrixQ,initPowers_List]\n generates a WeightedStochasticBasis object for state and non state variables and polynomial ranges and powers"
FindMaxError::usage="FindMaxError[polys_List,varRanges_List]\n uses NMaximize to find largest absolute value of functions over the given range\n see ReplaceVariables for subbing system of equation with chebyshev polynomials"
ReplaceVariables::usage="ReplaceVariables[theMod_,thePolys_List,{state_List,nonState_List}]\n substitutes chebyshev polynomials for the state and non state variables in the model equations"
CreatePolynomials::usage="CreatePolynomials[results_?JavaObjectQ]\n Pure mathematica function that uses ProjectionMethodResults to generate a list of the tensor product of the chebyshev polynomials in 'ProjectionMethodToolsJava order"
PlotPolynomials::usage="PlotPolynomials[polys_List,varVals_List]"
GenerateModelCode::usage="GenerateModelCode[theModel_Symbol]"
chebyshevExtrema::usage="chebyshevExtrema[nn_Integer]";

chebyshevNodes::usage = "chebyshevNodes[nn_Integer]\n pure mathematica functions that computes the chebyshev nodes (to infinite precision)"
(*
genPolys::usage = "genPolys[theWts_?MatrixQ,theStateVars_List,theRanges_?MatrixQ,theOrds_?VectorQ]\n Pure mathematica function that generates a list of the tensor product of the chebyshev polynomials in 'ProjectionMethodToolsJava order"
nearestLQ::usage="nearestLQ[theSolnSubs_List,thePhi_?NumberQ,thePoly_,theSubs_List]"
*)
fromChebyshevInterval::usage = "fromChebyshevInterval[xVal_,theMin_,theMax_]"
    theMin+((xVal+1)*(theMax-theMin)/2)
ComputeInitialCollocationWeights::usage=
"ComputeCollocationWeights[basis,initWts,modEqns]"
ComputeCollocationWeightsToOrder::usage=
"ComputeCollocationWeightsToOrder[previousResult]"
toChebyshevInterval::usage = "toChebyshevInterval[aVal_,theMin_,theMax_] "

getAllNewtonIterInfo::usage="get newton method iteration info"

genModGrid::usage = "genModGrid[theModel_Symbol,{{AALow_?NumberQ,AAHigh_?NumberQ}, {bigDeltaLow_?NumberQ, bigDeltaHigh_?NumberQ}},{AAPow_?NumberQ, bigDeltaPow_?NumberQ}]"
getPiEtcForPhiWider::usage = "getPiEtcForPhiWider[thePhi_?NumberQ,theModel_Symbol,someSubs_List,polyRanges_List,polyPowers_List,modEqns_?JavaObjectQ,theRange_?NumberQ,wtFunc_Symbol] :=
    "
(*benchMarkSubs::usage = "substitutions for benchmark case";*)(* Mathematica package *)
(* Exported symbols added here with SymbolName::usage *) 
Print["making x$4$Poly global for displaying polynomials"]
Global`x$4$Poly::usage = "for displaying polynomial";
ProtectedSymbols`t::usage = "the time variable";
Global`pow::usage ="so that pow can be used in codeGenSubs without long private name"
(*benchMarkSubs::usage = "substitutions for benchmark case";*)
newWeightedStochasticBasis::usage = "associate equations with model name";
projEquations::usage = "model equations associated with symbol";
(*windowsQ::usage="running windows?"*)
phiFunc::usage="phiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer,aa_|(aa_|aa_?NumberQ),bb_|(bb_|bb_?NumberQ)]" 
computeChebCoeffs::usage="computeChebCoeffs[theFunc_, thePows_List, theRanges_List]"

doJavac::usage = "doJavac[fName+String] compiles the ProjectionMethodToolsJava model (expects it to be a subclass of DoEqns "


getAllXVals::usage="getAllXVals[theRes_?JavaObjectQ]"
getAllFVals::usage="getAllFVals[theRes_?JavaObjectQ]"
getAllDFVals::usage="getAllDFVals[theRes_?JavaObjectQ]"

gtInitWeights::usage="gtInitWeights[aRes_?JavaObjectQ]:"


gtOrders::usage="gtOrders[aRes_?JavaObjectQ]:"


gtParams::usage="gtParams[aRes_?JavaObjectQ]:"


gtRanges::usage="gtRanges[aRes_?JavaObjectQ]:"




evalMod::usage="evalMod[aMod_?JavaObjectQ,aWSB_?JavaObjectQ,aMat_?MatrixQ]"



gtPolyOrdersForOuterProduct::usage="gtPolyOrdersForOuterProduct[aWSB_?JavaObjectQ]:"


gtPolyRanges::usage="gtPolyRanges[aWSB_?JavaObjectQ]:"


gtStateVars::usage="gtStateVars[aWSB_?JavaObjectQ]:"
gtShockVars::usage="gtStateVars[aWSB_?JavaObjectQ]:"
gtStateVarsNoShocks::usage="gtStateVarsNoShocks[aWSB_?JavaObjectQ]"




gtNonStateVars::usage="gtNonStateVars[aWSB_?JavaObjectQ]:"



gtPolys::usage="gtPolys[aWSB_?JavaObjectQ]:"

mmaPolysAtPts::usage="mmaPolysAtPts[aWSB_?JavaObjectQ]:"


mmaPolysAtOtherPts::usage="mmaPolysAtOtherPts[aWSB_?JavaObjectQ,otherPts_?MatrixQ]:"



gtPolysAtPts::usage="gtPolysAtPts[aWSB_?JavaObjectQ]:"



gtXFormedChebSubs::usage="gtXFormedChebSubs[aWSB_?JavaObjectQ]"



otherChebSubs::usage="otherChebSubs[aWSB_?JavaObjectQ,otherVals_?MatrixQ]"



gtXFormedChebNodes::usage="gtXFormedChebNodes[aWSB_?JavaObjectQ]:"


gtGrid::usage="gtGrid[aWSB_?JavaObjectQ]:"


gtStateVarsTimeTAtNodes::usage="gtStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"





mmaStateVarsTimeTAtNodes::usage="mmaStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"



mmaStateVarsTimeTP1AtNodes::usage="mmaStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"




gtStateVarsTimeTP1AtNodes::usage="gtStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"


gtNStateVarsTimeTAtNodes::usage="gtNStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"


gtNStateVarsTimeTP1AtNodes::usage="gtNStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:"


gtStateVarPolys::usage="gtStateVarPolys[aWSB_?JavaObjectQ]:"

gtNStateVarPolys::usage="gtNStateVarPolys[aWSB_?JavaObjectQ]:"


gtResWeights::usage="gtResWeights[aRes_?JavaObjectQ]:"



debugEqCode::usage="debugEqCode[modelName_String,eqns_List,svInfo_List]"
solnCondsToPiecewise::usage="solnCondsToPiecewise[solnWithConds_List]"
Print["beginning private defs"]
Begin["`Private`"]

solnCondsToPiecewise[solnWithConds:{{HoldPattern[_->ConditionalExpression[__]]..}..}]:=
With[{sConds=Sort[Flatten[solnWithConds]]},
With[{toSub=Union[First/@sConds]},
With[{grp=Cases[sConds,HoldPattern[#->_]]&/@toSub},
Thread[toSub->((Piecewise[List @@@ Last /@ #]//FullSimplify)&/@grp)]]]]
(*
solnCondsToPiecewise[xx_]:=Identity[xx]
*)
gtXFormedChebNodes[aWSB_?JavaObjectQ]:=With[{theSt=gtStateVarPolys[aWSB]},
theSt[getXformedChebNodePts[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtGrid[aWSB_?JavaObjectQ]:=With[{theSt=gtStateVarPolys[aWSB]},
theSt[getTheGrid[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
With[{theState=gtStateVarPolys[aWSB]},
Module[{updt=theState[setTheWeights[aMat]]},
theState[getVariablesAtChebyshevNodesTimeT[]][getArray[]]]
]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


mmaStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
With[{theState=mmaPolysAtPts[aWSB]},If[aWSB[isPerfectForesightQ[]],
Transpose[aMat. theState],
ArrayFlatten[{{Transpose[aMat. theState],ConstantArray[0,{Length[theState[[1]]],1}]}}]]
]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


mmaStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
Transpose[aMat.mmaPolysAtOtherPts[aWSB,mmaStateVarsTimeTAtNodes[aWSB,aMat]]]

(*
With[{theState=mmaPolysAtPts[aWSB]},
ArrayFlatten[{{Transpose[aMat. theState],ConstantArray[0,{Length[theState[[1]]],1}]}}]
]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

*)


gtStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
With[{theState=gtStateVarPolys[aWSB]},
Module[{updt=theState[setTheWeights[aMat]]},
theState[getVariablesIteratedFromChebNodesTimeTP1[]][getArray[]]]
]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


gtNStateVarsTimeTAtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
With[{theNS=gtNStateVarPolys[aWSB]},
If[theNS===Null,{{}},
With[{theState=gtStateVarPolys[aWSB]},
Module[{updt=aWSB[setAllWeights[aMat]]},
theState[getVariablesAtChebyshevNodesTimeTNSP[]][getArray[]]]
]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtNStateVarsTimeTP1AtNodes[aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
With[{theNS=gtNStateVarPolys[aWSB]},
If[theNS===Null,{{}},
With[{theState=gtStateVarPolys[aWSB]},
Module[{updt=aWSB[setAllWeights[aMat]]},
theState[getVariablesIteratedFromChebNodesTimeTP1NSP[]][getArray[]]]
]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtStateVarPolys[aWSB_?JavaObjectQ]:=aWSB[getTheState[]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtNStateVarPolys[aWSB_?JavaObjectQ]:=aWSB[getTheNonState[]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtResWeights[aRes_?JavaObjectQ]:=aRes[Global`getResWeights[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtTheStrategyIters[aRes_?JavaObjectQ]:=aRes[getTheStrategyIters[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtTheSystem[aRes_?JavaObjectQ]:=aRes[getTheSystem[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtTheWeightedStochasticBasis[aRes_?JavaObjectQ]:=aRes[getTheWeightedStochasticBasis[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"




gtChebNodes[aWSB_?JavaObjectQ]:=With[{theSt=gtStateVarPolys[aWSB]},
theSt[getXformedChebNodePts[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtPolyOrdersForOuterProduct[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
theGr[generatePolyOrdersForOuterProduct[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtPolyRanges[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
theGr[getRanges[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtStateVars[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
theGr[getVariableNames[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtStateVarsNoShocks[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
With[{withoutShocksDim=theGr[getStateDimWithoutShocks[]]},
theGr[getVariableNames[]][[Range[withoutShocksDim]]]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


gtShockVars[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
With[{withoutShocksDim=theGr[getStateDimWithoutShocks[]]},
Drop[theGr[getVariableNames[]],withoutShocksDim]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"





gtNonStateVars[aWSB_?JavaObjectQ]:=With[
{theNS=aWSB[getTheNonState[]]},
theNS[getNonStateVariableNames[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"



gtPolys[aWSB_?JavaObjectQ]:=With[{rngs=gtPolyRanges[aWSB],
theOrds=gtPolyOrdersForOuterProduct[aWSB],
theVars=ToExpression/@gtStateVars[aWSB]},
With[{xxVars=Table[Unique["anX"],{Length[theVars]}]},
With[{funcList=
MapThread[onePhiFunc[#2,#3,#1[[1]],#1[[2]]]&,{rngs,theVars,xxVars}]},
With[{phiFuncs=Function@@ {xxVars,funcList}},
(Times @@ #)&/@((phiFuncs @@ #)& /@ theOrds)
]]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"



mmaPolysAtPts[aWSB_?JavaObjectQ]:=With[{thePolys=gtPolys[aWSB],
theSubs=gtXFormedChebSubs[aWSB]},
Transpose[thePolys/.theSubs]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

mmaPolysAtOtherPts[aWSB_?JavaObjectQ,otherPts_?MatrixQ]:=
With[{thePolys=gtPolys[aWSB],
theSubs=otherChebSubs[aWSB,otherPts]},
Transpose[thePolys/.theSubs]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

(*
mmaPolysAtTP1Pts[aWSB_?JavaObjectQ]:=With[{thePolys=gtPolys[aWSB],
theSubs=gtXFormedChebSubs[aWSB]},
Transpose[thePolys/.theSubs]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"
*)

gtPolysAtPts[aWSB_?JavaObjectQ]:=With[{theGr=gtGrid[aWSB]},
theGr[getBasisAtChebNodes[]]]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"






gtInitWeights[aRes_?JavaObjectQ]:=aRes[getInitWeights[]]/;
  ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtOrders[aRes_?JavaObjectQ]:=aRes[getOrders[]]/;
 ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtParams[aRes_?JavaObjectQ]:=aRes[getParams[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"
gtRanges[aRes_?JavaObjectQ]:=aRes[getRanges[]]/;
ClassName[aRes]==="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"




gtXFormedChebSubs[aWSB_?JavaObjectQ]:=With[{theVals=gtXFormedChebNodes[aWSB],
theVars=gtStateVars[aWSB]},
Function[xxx,Transpose[{theVars,xxx}]/.List[xx_String,yy_]->Rule[xx,yy]]/@theVals]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"

gtXFormedChebSubsNotStrings[aWSB_?JavaObjectQ]:=With[{theVals=gtXFormedChebNodes[aWSB],
theVars=ToExpression /@gtStateVars[aWSB]},
Function[xxx,Transpose[{theVars,xxx}]/.List[xx_Symbol,yy_]->Rule[xx,yy]]/@theVals]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


otherChebSubs[aWSB_?JavaObjectQ,otherVals_?MatrixQ]:=
With[{theVars=gtStateVars[aWSB]},
Function[xxx,Transpose[{theVars,xxx}]/.List[xx_String,yy_]->Rule[xx,yy]]/@ otherVals]/;
ClassName[aWSB]==="gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis"


(* Implementation of the package *)

evalMod[aMod_?JavaObjectQ,aWSB_?JavaObjectQ,aMat_?MatrixQ]:=
Module[{eqValDrv},
aWSB[setAllWeights[aMat]];
eqValDrv=aMod[updateValDrv[aWSB]];
{eqValDrv[getTheVal[]][getArray[]],eqValDrv[getTheJac[]][getArray[]]}]



stateVariables::usage ="stateVariables[modelEquations_]" 
(*
Created on 2009/02/05

Imported from
S:\learnProjection\proto\authoritative\refactorProj.nb
*)

$phiIncrement = 0.1; 
$rngIncrement = 0.01;

$goodBeta = 0.98;
$betaSteps = 1;
(*java reinstallation for rsma network eclipse*)





getAllNewtonIterInfo[theRes_?JavaObjectQ]:=
With[{stratIterInfo=gtTheStrategyIters[theRes]},
With[{newtIterInfo=(#[toArray[]])&/@stratIterInfo[toArray[]]},
Map[allNewt,newtIterInfo,{2}]]]/;ClassName[theRes]===
"gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"

getAllXVals[theRes_?JavaObjectQ]:=
With[{allInfo=getAllNewtonIterInfo[theRes]},
allInfo[[All,All,2]]]

getAllFVals[theRes_?JavaObjectQ]:=
With[{allInfo=getAllNewtonIterInfo[theRes]},
allInfo[[All,All,3]]]

getAllDFVals[theRes_?JavaObjectQ]:=
With[{allInfo=getAllNewtonIterInfo[theRes]},
allInfo[[All,All,4]]]

getAllDFVals[theRes_?JavaObjectQ]:=
With[{allInfo=getAllNewtonIterInfo[theRes]},
allInfo[[All,All,4]]]

allNewt[xx_?JavaObjectQ]:=xx[#] & /@ {getDelta[], getXx[], getfVal[],getDfVal[],getLambda[],getShrinkFactorNow[]}

nearestOne[theSubs_List] :=
    With[ {theReals = allReals[theSubs]},
        First[Sort[theReals,(Abs[#[[2]]-1]<Abs[#2[[2]]-1])&]]
    ]


nearestLQ[theSolnSubs_List,thePhi_?NumberQ,thePoly_,theSubs_List] :=
    With[ {theReals = allReals[theSolnSubs]},If[Length[theReals]==1,theReals,
        With[ {piLQ = (tangentLineFunc[thePhi]//.Global`graphSubs)//.theSubs,thePis = thePoly[[6]]/.theReals},
            With[ {theDiffs = Flatten[{Abs[thePis-piLQ]}]},(*Print[{thePis,piLQ,theDiffs}];*)
                With[ {minPos = First[Ordering[Flatten[{theDiffs}]]]},
                    theReals[[minPos]]
                ]
            ]
        ]
    ]]
 

graphSubs={kappa->((1-alpha)*(1-alpha*beta)*(sigma+chi)/alpha)*(1/(1+chi*anEps)),lambda->kappa/anEps};
   
tangentLineFunc = 
  Function @@ {ProtectedSymbols`thePhi, 
    1 + (Global`kappa*Global`lambda/((1 - Global`beta)*Global`lambda + Global`kappa^2)*
         ProtectedSymbols`thePhi/(Global`sigma + Global`chi) //. graphSubs) }

tOrtp1[exprs_] :=
    Union[Select[exprs,Not[And[FreeQ[#,xx_[ProtectedSymbols`t+1]],FreeQ[#,xx_[ProtectedSymbols`t]]]]&]]
logCnstrns[eqns_List] :=
    (-1)*
        tOrtp1[Cases[eqns,Log[xx_]->xx,Infinity]]
  
 varCnstrns[aProjMod_?JavaObjectQ] :=
     With[ {sVars = 
     	Through[ToExpression[aProjMod[getTheState[]][getStateVariableNames[]]][ProtectedSymbols`t]],sVarstp1 = 
     	Through[ToExpression[aProjMod[getTheState[]][getStateVariableNames[]]][ProtectedSymbols`t+1]],
     	theMax = aProjMod[getTheState[]][getTheGrid[]][theChebPoly][theMax],
         theMin = aProjMod[getTheState[]][getTheGrid[]][theChebPoly][theMin]},
         Join[Join[sVars-theMax,-sVars+theMin](*,Join[sVarstp1-theMax,-sVarstp1+theMin]*)]
     ]
  
powCnstrns[eqns_List] :=
    (-1)*
        tOrtp1[Cases[eqns,Power[xx_,yy_]->xx,Infinity]]
    
    preAllCnstrns[eqns_List,aProjMod_?JavaObjectQ] :=
        Union[logCnstrns[eqns],powCnstrns[eqns],
        	varCnstrns[aProjMod]]
        	
    elimCnstrns[someCnstrns_List]:=
    With[{ineq=And@@Thread[someCnstrns<=0]},
    	With[{fewer=FullSimplify[ineq]},
    	(List @@fewer)/.{xx_<=yy_->xx-yy,xx_>=yy_->yy-xx,
    		Inequality[xx_,LessEqual,yy_,LessEqual,zz_]->Sequence[xx-yy,yy-zz],
    		Inequality[xx_,GreaterEqual,yy_,GreaterEqual,zz_]->Sequence[zz-yy,yy-xx]}]]
    allCnstrns[eqns_List,aProjMod_?JavaObjectQ]:=
    elimCnstrns[preAllCnstrns[eqns,aProjMod]]
    	
    makeDoEqnsCnstrnsCode[theModel_Symbol,aProjMod_?JavaObjectQ] :=
        With[ {eqns = allCnstrns[projEquations[theModel],aProjMod],snsVars = getStateNonState[theModel]},
            With[ {eqnsCode = doEqCodeSubs[ToString[theModel]<>"Cnstrns",eqns,snsVars]},
                eqnsCode
            ]
        ]/;And[projLags[theModel]<= 1,projLeads[theModel]<= 1]

allReals[theSubs_List] :=
    With[ {theReals = Select[theSubs,(And @@ (chkSub/@ #))&]},
        theReals
    ]
chkSub[aSub:xx_->yy_]:=And[Im[yy]==0,yy>=1]


doRng[pm_?JavaObjectQ,sml_?NumberQ,
	anInit_?MatrixQ,modEqns_?JavaObjectQ] :=
	Module[ {},
		With[{},
  {pm[collocateProjWtsMiddle[sml,anInit, modEqns]],{{}}}]]
	
(*	
kronProd[theStateVars_List,rngs_List,pws_List]:=
Module[{aPolys,dPolys,gPolys,thePolys},
	thePolys=MapThread[phiFunc[#3,#1,#2[[1]],#2[[2]]]&,
		{pws,rngs,theStateVars}];(*Print[thePolys];*)
Flatten[Outer[Times,Sequence@@thePolys]
	]];
*)
	
doBigRng[theModel_Symbol,smlLow_?NumberQ,smlHigh_?NumberQ,polyRanges_List,polyPowers_List,anInit_?MatrixQ,modEqns_?JavaObjectQ] :=
    Module[ {highRes,highPoly},
    	With[{snsVarsStrs = 
    		Map[ToString,getStateNonState[theModel],{-1}]},
		With[{pm = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",snsVarsStrs[[1]],snsVarsStrs[[2]], 
  polyRanges,polyPowers]},
    {highRes,highPoly} = Catch[ doRng[pm,smlHigh,anInit,modEqns],nada,ffDoBigRng];
 If[ MatrixQ[highRes],
            {smlHigh,{highRes,highPoly}},
            With[ {midVal = (smlHigh+smlLow)/2},
                doBigRng[theModel,smlLow,midVal,polyRanges,polyPowers,anInit,modEqns]
            ]
        ]
    ]]]
ffDoBigRng[xx___]:=
	{{xx},{xx}}

iterRng[pm_?JavaObjectQ,anInit_?MatrixQ,modEqns_?JavaObjectQ]:=
{pm[collocateFullWidth[anInit,modEqns]],{{}}}

phiSneak[thePhi_?NumberQ] :=
    {thePhi}
   (* Sign[thePhi]*Range[0,Abs[thePhi],$phiIncrement]*)

phiStep[pm_?JavaObjectQ,
	phiNow_?NumberQ,someSubs_List,someWts_?MatrixQ,modEqns_?JavaObjectQ] :=
    Module[ {},
    	With[{},
		With[{},
        Global`setParams[phiNow,someSubs,modEqns];
        {nxt00,theRes00} = doRng[pm,$rngIncrement,someWts[[All,Range[1]]],modEqns];
        nxt00
    ]]]


closeOnBeta[pm_?JavaObjectQ,phiNow_?NumberQ,someSubs_List,someWts_?MatrixQ,
	modEqns_?JavaObjectQ] :=
    With[ {targetBeta = Global`beta/.someSubs},
    	With[{},
		With[{},
        With[ {incSubs = someSubs/.{HoldPattern[Global`beta->_]:> 
        	Global`beta->#}&/@
        	Range[$goodBeta,targetBeta,(targetBeta-$goodBeta)/$betaSteps]},
           Catch[Fold[phiStep[pm,phiNow,#2,#1,modEqns]&,
           	someWts,incSubs],nada,ffCloseOn]
        ]
    ]]]
    
    closeOnBeta[pm_?JavaObjectQ,phiNow_?NumberQ,someSubs_List,someWts_?MatrixQ,
	modEqns_?JavaObjectQ] :=
	With[{params={Global`alpha,Global`anEps,Global`beta,Global`chi,Global`rho,Global`rhoG,Global`sigma,ProtectedSymbols`tau}},
	With[{target=N[((params/.someSubs)/.ProtectedSymbols`tau->(1-phiNow))],away=N[(((params/.Global`beta->$goodBeta)/.someSubs)/.ProtectedSymbols`tau->(1-phiNow))]},
pm[collocateParamsTarget[someWts,modEqns,away,target]]]]
    
foldDoRng[pm_?JavaObjectQ,thePhi_?NumberQ,someSubs_List,modEqns_?JavaObjectQ,wtFunc_Symbol,polyPowers_List] :=
    Module[ {initWts,powNow,lstPow,lstWts,wtsNow},
    	With[{},
		With[{},powNow={0,0,0};
        Global`setParams[thePhi,someSubs,modEqns];
        initWts = phiWts[pm,thePhi,someSubs,
        	modEqns,wtFunc];
        	Sow[{initWts,"foldDoRng"},"foldDoRng"];
        	wtsNow=doRng[pm,$rngIncrement,
        		initWts[[All,Range[1]]],modEqns];
        		While[powNow =!=polyPowers,
        			lstPow=powNow;lstWts=wtsNow;
        			powNow=MapThread[Min[#1,#2+1]&,{polyPowers,powNow}];
        		wtsNow=nxtWts[lstWts[[1]],lstPow,powNow];
           	pm = pm[incPowers[polyPowers]];
    wtsNow=doRng[pm,$rngIncrement,wtsNow,modEqns]];
     wtsNow[[1]]
        		]]]
phiWts[pm_?JavaObjectQ,phiTarg_?NumberQ,  someSubs_List,
	modEqns_?JavaObjectQ,wtFunc_Symbol] :=
    With[ {
    	someWts = 
       wtFunc[1, someSubs ]},
       With[{},
		With[{},
       With[{almostDone= Fold[closeOnBeta[pm,#2,  someSubs, #1,
        	modEqns] &, someWts, 
         phiSneak[phiTarg]]},
         ArrayFlatten[{{almostDone,
         	ConstantArray[0,{Length[someWts],1(*(Times @@ (polyPowers+1))-1*)}]}}]
    ]]]]
(* 
getPiEtcForPhiWider[theRes_?JavaObjectQ,someSubs_List]:=
With[{theRanges=theRes[getRanges[]],thePows=theRes[getOrders[]],thePhi=1-(theRes[getParams[]])[[-1]]},
    With[{thePolys=genPolys[Chop[theRes@resWeights], {ProtectedSymbols`theAA,ProtectedSymbols`theAA$Shock, ProtectedSymbols`theBigDelta,
    	ProtectedSymbols`theGG,ProtectedSymbols`theGG$Shock}, 
   theRanges, thePows] // Expand}, 
   With[{theSoln = Solve[thePolys[[3]]== ProtectedSymbols`theBigDelta,ProtectedSymbols`theBigDelta]},
   	With[{
   	                    theDelta=ProtectedSymbols`theBigDelta/.nearestLQ[theSoln,thePhi,thePolys,someSubs]},
                With[{thePi = thePolys[[6]]/.nearestLQ[theSoln,thePhi,thePolys,someSubs]},
                    (*Print["thePhi=",thePhi," pibar=",thePi,"theDelta=",theDelta];Print["done order "];*)
                {thePolys,theSoln,thePi}]]]]]
    
getPiEtcForPhiVary[thePhi_?NumberQ,theModel_Symbol,someSubs_List,polyRanges_List,polyPowers_List,wtFunc_Symbol,modEqns_?JavaObjectQ] :=
    Module[ {realpm = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",snsVarsStrs[[1]],snsVarsStrs[[2]], 
  polyRanges,0*polyPowers]},
    	With[{snsVarsStrs = Map[ToString,getStateNonState[theModel],{-1}]},
		With[{pm = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",snsVarsStrs[[1]],snsVarsStrs[[2]], 
  polyRanges,0*polyPowers]},
        With[ {initForPhi = foldDoRng[pm,thePhi,someSubs,modEqns,wtFunc,polyPowers]},(*Print["done initForPhi",initForPhi];*)
            With[ {},
                Global`setParams[thePhi,someSubs,modEqns];
                With[ {allVals = iterRng[realpm,
                	initForPhi,modEqns]},(*Print["done allVals"];*)
                    thePoly = allVals[[-1,2,2]];
                    theSoln = Flatten[Solve[thePoly[[2,1]]== Global`xx,{Global`xx}]];
                    theDelta=xx/.nearestLQ[theSoln,thePhi,thePoly,someSubs];
                    thePi = thePoly[[3,1]]/.nearestLQ[theSoln,thePhi,thePoly,someSubs];
                    (*Print["thePhi=",thePhi," pibar=",thePi,"deltaBar=",theDelta];*)(*Print["done order "];*)
                    {thePoly,theSoln,thePi}
                ]
            ]
        ]
    ]]]
*)
(*genPolys[theWts_?MatrixQ,theStateVars_List,theRanges_?MatrixQ,theOrds_?VectorQ]:=
Module[{},(*Print[theWts,theStateVars,theRanges,theOrds];*)
Expand[theWts . kronProd[theStateVars,theRanges,theOrds]]
]
*)
(*
genPolysFromBasis[theBasis_?JavaObjectQ,theWts_?MatrixQ]:=
Module[{theState=theBasis[getTheState[]]},
	With[{vNames=ToExpression/@(theState[getStateVariableNames[]])},
		With[{grid=theState[getTheGrid[]]},
			With[{theOrds=grid[getTheOrders[]],vSpec=grid[getTheStateVars[]]},
								With[{
				theMins=vSpec[getMinVals[]],
				theMaxs=vSpec[getMaxVals[]]},
				With[{theMins=vSpec[getMinVals[]],
				theMaxs=vSpec[getMaxVals[]]},
				With[{reOrd=columnMap[grid[
generatePolyOrdersForOuterProduct[]],theOrds]},
With[{theOrdOut=getOrderedOuter[theBasis]},
Print[theMins,theMaxs,reOrd,theOrdOut];
Expand[(theWts(*[[All,reOrd]]*)) . theOrdOut]]]]]]]]]
				
*)
prodHelper01[polys_List,ords_List]:=
With[{prep=MapIndexed[polys[[#2[[1]],#1+1]]&,ords]},Times @@ prep]



getOrderedOuterString[theBasis_?JavaObjectQ]:=
Module[{theStatePoly=theBasis[getTheState[]]},
Module[{vNames=theStatePoly[getStateVariableNames[]],
thePows=theStatePoly[getTheGrid[][generatePolyOrdersForOuterProduct[]]]},
With[{thePolys=getPhiFunc[#,theBasis]&/@theStatePoly[getStateVariableNames[]]},
With[{theRes=Expand[prodHelper01[thePolys,#]&/@ thePows]},
theRes]]]]

getOrderedOuter[theBasis_?JavaObjectQ]:=
Module[{theStatePoly=theBasis[getTheState[]]},
Module[{thePows=theStatePoly[getTheGrid[][generatePolyOrdersForOuterProduct[]]]},
With[{thePolys=getPhiFunc[#,theBasis]&/@(theStatePoly[getStateVariableNames[]])},
With[{theRes=Expand[prodHelper01[thePolys,#]&/@ thePows]},
theRes]]]]

(*
genPolys[theBasis_?JavaObjectQ,theWts_?MatrixQ]:=
Module[{theStatePoly},
With[{theStatePoly=theBasis[getTheState[]]},
Module[{vNames=theStatePoly[getStateVariableNames[]],
thePows=theStatePoly[getTheGrid[][generatePolyOrdersForOuterProduct[]]]},
With[{vNameSubs=Thread[vNames->(ToExpression/@vNames)],
thePolys=getPhiFunc[#,theBasis]&/@theStatePoly[getStateVariableNames[]]},
With[{theRes=Expand[prodHelper01[thePolys,#]&/@ thePows]},
theWts . theRes/.vNameSubs]]]]]

prodHelper01[polys_List,ords_List]:=
With[{prep=MapIndexed[polys[[#2[[1]],#1+1]]&,ords]},Times @@ prep]

*)
varNumberState[vName_String,aBasis_?JavaObjectQ]:=
Position[
Which[
stateVarQ[vName,aBasis],gtStateVars[aBasis],
True,Throw[vName,"-varNumber:variable not found in state-"]],vName][[1,1]]



getPhiFunc[vName_String,theBasis_?JavaObjectQ]:=
Module[{theStatePoly=theBasis[getTheState[]],
vNumber=varNumberState[vName,theBasis]},
With[{rng=theStatePoly[getRanges[]][[vNumber]],
ord=theStatePoly[getOrders[]][[vNumber]]},
phiFunc[vName,ord,rng[[1]],rng[[2]]]//Expand//Chop]]

getRawPhiFunc[vName_String,theBasis_?JavaObjectQ]:=
Module[{theStatePoly=theBasis[getTheState[]],
vNumber=varNumberState[vName,theBasis]},
With[{ord=theStatePoly[getOrders[]][[vNumber]]},
phiFunc[vName,ord,-1,1]]//Expand//Chop]


columnMap[spOrds_List,theOrds_List]:=
With[{fromOuter=Flatten[Outer@@ Prepend[(Range[0,#]&/@theOrds),List],Length[theOrds]-1]},(*Print[fromOuter];*)
Flatten[getPos[#,spOrds]&/@fromOuter]]
	

getPos[anOrd_List,spOrds_List]:=
First[Position[spOrds,anOrd]]


$highOrder = 20;
doAValue[theVal_?NumberQ,theModel_Symbol,someSubs_List] :=
    doAValue[theVal,someSubs] = With[ {theRes = getPiEtcForPhiWider[theVal,theModel,someSubs,$highOrder]},
                                    theRes
                                ]
toDoSomeValues[theVal_?NumberQ] :=
    Last[nearestOne[doAValue[theVal,someSubs][[3]]]]
(**)
applyNew[phiNow_?NumberQ,theOrders_List,newModEqns_?JavaObjectQ,newModCnstrnsEqns_?JavaObjectQ]:=
Module[{
targParams = ({Global`alpha, Global`anEps, Global`beta, Global`chi, Global`rho, Global`rhoG, Global`sigma, ProtectedSymbols`tau} /. 
     Global`benchmarkSubs) /. ProtectedSymbols`tau -> (1 - phiNow),
initWts = {{1.0005988400895465`}, {0.`}, {1.148176777917186`}, 
{1.000020512737284`}, {0.`}, {0.5768194669188176`}, 
{0.8281106706816505`}, {1.0037721741765704`}, {0.9502868586024235`}, 
{0.`}, {0.8748376336677929`}, {1.1486853484776833`}, 
{-0.04827728787002833`}, {0.05512860860967907`}, 
{-0.8301308596872036`}, {-0.055139840889132076`}, 
{0.`}},
initRngs = {{1.0, 1.1}, {-.00001, .00001}, {1, 3}, {1.0, 1.1}, {-.00001, .00001}},
svs = {"AA", "AA$Shock", "bigDelta", "GG", "GG$Shock"},
nsvs=	{"bigPi", "CC", "FF", "HH", "RR", "SS", "varphi1", "varphi2", 
"varphi3", "varphi4", "varphi5", "varphi6"}},
newModEqns[updateParams[targParams]];
pm = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis", svs, nsvs, initRngs, {0, 0, 0, 0, 0}];
vf = JavaNew["gov.frb.ma.msu.gsmin.experTackVecFun", newModEqns, 
   newModCnstrnsEqns, pm];
(*   aMat = JavaNew["Jama.Matrix", initWts];
theVD = vf[evaluate[aMat]];
theSS = vf[evaluateSumSq[aMat]];
theCn = vf[evaluateCnstrns[aMat]];
try = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.gsmMinProb", initWts, vf];
try[iterateToConvergence[.000001]];
peek[];
theRes = try[xNow];
newInit = theRes[getArray[]];*)newInit=initWts;
res00 = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults", pm, newInit, newModEqns];
Prent["res00converged?",res00[convergedQ]];
res01 = res00[incOrder[{0, 0, 1, 0, 0}]];
res02 = res01[incOrder[{0, 0, 1, 0, 0}]];
res03 = res02[incOrder[{0, 0, 1, 0, 0}]];
res04 = res03[incOrder[{0, 0, 1, 0, 0}]];
res05 = res04[incOrder[{0, 0, 1, 0, 0}]];
Print["res05converged?",res05[convergedQ]];
resEnd=res05[toOrder[getTheOrders[]]];
getPiEtcForPhiWider[resEnd, Global`benchmarkSubs]
]

peek[] := With[{xn = try[xNow]}, Print["xNow=", xn[getArray[]]]]


makeVarSpec[var_String,low_?NumberQ,high_?NumberQ] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec",var,low,high];

makeVarSpecs[theStateVars_] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec",theStateVars]/;VectorQ[theStateVars,varSpecQ]
varSpecQ = "gov.frb.ma.msu.ProjectionMethodToolsJava.varSpec"== ClassName[#]&;

makeGrid[vars_,ords_] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec",vars,ords]/;And[varSpecsQ[vars],VectorQ[ords,IntegerQ]]
varSpecsQ = ClassName[#]== "gov.frb.ma.msu.ProjectionMethodToolsJava.varSpecs"&;

makeStatePoly[aGrid_,initWts_] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials",aGrid,initWts]/;And[gridSpecQ[aGrid],MatrixQ[initWts,NumberQ]]
gridSpecQ = ClassName[#]== "gov.frb.ma.msu.ProjectionMethodToolsJava.gridSpec"&;
(*
makeNonStatePoly[aState_,nsWts_,nsNames_] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.nonStatePoly",aState,nsWts,nsNames]/;And[statePolyQ[aState],MatrixQ[nsWts,NumberQ],VectorQ[nsNames,StringQ]]
statePolyQ = ClassName[#]== "gov.frb.ma.msu.ProjectionMethodToolsJava.statePoly"&;
nonStatePolyQ = ClassName[#]== "gov.frb.ma.msu.ProjectionMethodToolsJava.nonStatePoly"&;
aPolyQ[xx_] :=
    Or[statePolyQ[xx],nonStatePolyQ[xx]]
*)
makeWeightedStochasticBasis[aPoly_?aPolyQ] :=
    JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",aPoly]


(*
[aThing___] :=
    Cases[{aThing},EquationValDrv[___]|_Symbol[ProtectedSymbols`t-1]|_Symbol[ProtectedSymbols`t]|_Symbol[ProtectedSymbols`t+1]|_?NumberQ,Infinity]==={}
*)
(*
Print["why not numbers?"]
noVars[aThing___] :=
    Cases[{aThing},EquationValDrv[___]|_Symbol[ProtectedSymbols`t-1]|_Symbol[ProtectedSymbols`t]|_Symbol[ProtectedSymbols`t+1]|EquationValDrv,Infinity,Heads->True]==={}
*)
noVars[aThing___]:=Or[NumberQ[aThing],FreeQ[aThing,EquationValDrv[___]|
	_Symbol[ProtectedSymbols`t-1]|
	_Symbol[ProtectedSymbols`t]|
	_Symbol[ProtectedSymbols`t+1]|
	Less|LessEqual|Greater|GreaterEqual|Global`eqvdIf|List|
	EquationValDrv]]/;Not[Head[aThing]===String]

condPairs[aStr_String]:=EquationValDrv[aStr]

myDeleteFile[fileName_String] :=
    With[ {fns = FileNames[fileName]},
        If[ Length[fns]>0,
            DeleteFile[fileName]
        ]
    ]


newWeightedStochasticBasis[model_Symbol,eqns_List] :=
    Block[ {},
        Clear[model];
        projEquations[model]^=Expand[eqns];
        myDeleteFile[ToString[model]<>".java"];
        myDeleteFile[ToString[model]<>".class"]
    ]

(*projEquations[model]^=eqns;*)
lagsLeads[modelEquations_] :=
    Union[Join[Cases[modelEquations,x_[ProtectedSymbols`t_]->0,Infinity],Cases[modelEquations,x_[ProtectedSymbols`t+v_]->v,Infinity]]];



projLags[model_] :=
    (projLags[model]^=(-1)*lagsLeads[projEquations[model]][[1]])/;projEquations[model]=!=projundefinedModel;
projLags[___] :=
    projundefinedModel;

projNeq[model_] :=
    Length[projEquations[model]]/;projEquations[model]=!=projundefinedModel;
projNeq[___] :=
    projundefinedModel;


projLeads[model_] :=
    (projLeads[model]^=lagsLeads[projEquations[model]][[-1]])/;projEquations[model]=!=projundefinedModel;
projLeads[___] :=
    projundefinedModel;


stateVariables[modelEquations_] :=
    Union[Cases[{modelEquations},x_[ProtectedSymbols`t]|x_[ProtectedSymbols`t+i_]->x,Infinity]]



reformatSNSVars[snsVars_SV] :=
    With[ {justState = DeleteCases[snsVars[[2]],ProtectedSymbols`eps[_][_]]/.xx_[_]->xx,
    	noEps = DeleteCases[snsVars[[1]],ProtectedSymbols`eps[_]|0]},
        With[ {nonState = Complement[noEps,justState]},
            {ToString/@justState,ToString/@nonState}
        ]
    ]
ComputeInitialCollocationWeights[basis_?JavaObjectQ,
	initWts_?MatrixQ,modEqns_?JavaObjectQ]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults", basis, initWts, modEqns];
ComputeCollocationWeightsToOrder[previousResult_?JavaObjectQ,
	targetOrders_List]:=
	previousResult[toOrder[targetOrders]];
	
	ComputeInitialCollocationWeights[basis_?JavaObjectQ,
	initWts_?MatrixQ,modEqns_?JavaObjectQ,strategy_?JavaObjectQ]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults", basis, initWts, modEqns,strategy];
ComputeCollocationWeightsToOrder[previousResult_?JavaObjectQ,
	targetOrders_List]:=
	previousResult[toOrder[targetOrders]];
	

(*for now, 
require constraints to involve 
nonState variables and constants for results*)



CreatePolynomials[results_?JavaObjectQ]:=
With[{basis=results[getTheWeightedStochasticBasis[]],
theWts=results[getResWeights[]]},
	With[{theState=basis[getTheState[]]},
		With[{grid=theState[getTheGrid[]]},
			With[{theOrds=grid[getTheOrders[]],vSpec=grid[getTheStateVars[]]},
				With[{
				theMins=vSpec[getMinVals[]],
				theMaxs=vSpec[getMaxVals[]]},
					Expand[(theWts .  gtPolys[basis])]//Chop]]]]]
					

PlotPolynomials[polys_List,varVals_List,varNames_List]:=With[{allPlots=
	MapThread[onePlot[#,varVals,#2]&,{polys,varNames}]},GraphicsGrid[{allPlots}]]
	
	
onePlot[aPoly_,varVals_List,varName_String]:=With[{
	polySubbed=doPolySub[aPoly,varVals]},
	With[{plotVars=getPlotVars[varVals]},
		Switch[Length[plotVars],
			1,Plot @@{polySubbed,plotVars[[1]],
				AxesLabel:>{makeSubscripted[plotVars[[1,1]],"t-1"],
					makeSubscripted[varName,"t"]}},
			2,Plot3D @@{ polySubbed,plotVars[[1]],plotVars[[2]]}]]]

FindMaxError[polys_List,varRanges_List]:=NMaximize[Abs[#],varRanges]&/@polys

FindMaxError[polys_List,cons_List,varRanges_List]:=NMaximize[{Abs[#],cons},varRanges]&/@polys
makeSubscripted[xx_,yy_]:=
ToExpression[
	"Subscript["<>
	ToString[xx]<>","<>
	ToString[yy]<>"]"]

doPolySub[aPoly_,varVals_List]:=
With[{subs=Cases[varVals,{xx_Symbol,yy_?NumberQ}->(xx->yy)]},
	aPoly/.Flatten[subs]]
getPlotVars[varVals_List]:=
Cases[varVals,{_Symbol,_?NumberQ,_?NumberQ},Infinity]
			
GenerateModelCode[theModel_Symbol] :=
    With[ {eqns = projEquations[theModel],snsVars = getStateNonState[theModel]},
    	myDeleteFile[ToString[theModel]<>".java"];
        myDeleteFile[ToString[theModel]<>".class"];
        With[ {eqnsCode = doEqCodeSubs[ToString[theModel],eqns,snsVars]},
(*Print["ingenmodcode:",eqnsCode,snsVars];*)
            {Map[(ToString[#])&,snsVars,{-1}],eqnsCode}(*was global`*)
            ]
    ]/;And[projLags[theModel]<= 1,projLeads[theModel]<= 1]
    
GenerateBasis[{stateVars_List,nonStateVars_List},
	initRanges_?MatrixQ,initPowers_List]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",stateVars,nonStateVars,
initRanges,initPowers]

GenerateBasis[stateVars_List,stateRanges_?MatrixQ,statePowers_List,
	shockVars_List,shockMeans_?VectorQ,shockStDevs_?VectorQ,intOrders_?VectorQ,shockPowers_List,nonStateVars_List]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",
	stateVars,stateRanges//N,statePowers,shockVars,shockMeans//N,shockStDevs//N,intOrders,shockPowers,nonStateVars]

GenerateBasis[projRes_?JavaObjectQ]:=
With[{theWSB=projRes[getTheWeightedStochasticBasis[]]},
With[{theState=theWSB[getTheState[]],theNonState=theWSB[getTheNonState[]]},
With[{numShocks=theState[getNumberOfShocks[]],
allState=theState[getStateVariableNames[]]},
With[{shockVars=Reverse[allState[[-Range[numShocks]]]]},
With[{stateVars=Complement[allState,shockVars],
stateRanges=theState[getRanges[]],
statePowers=theState[getOrders[]],
shockMeans=theWSB[getGaussHermiteMean[]],
shockStDevs=theWSB[getGaussHermiteStDev[]],
nonStateVars=theNonState[getNonStateVariableNames[]]},
GenerateBasis[stateVars,stateRanges,statePowers,
	shockVars,shockMeans,shockStDevs,intOrders,shockPowers,nonStateVars]]]]]]
	
	
	
Print["define preparsesubs"];
tryNow[eqns_List]:=
FixedPoint[compEqns,eqns]

compEqns[eqns_]:=Module[{},Print["h12"];
subOutEqvdIf[
		Print["h11"];
subOutConds[
		Print["h10"];
subOutDoInt[
		Print["h9"];
subOutErf[
		Print["h8"];
subOutPower[
		Print["h7"];
subOutPlus[
		Print["h6"];
subOutTimes[
		Print["h5"];
subOutRational[
		Print["h4"];
		Identity[
				Print["h3"];
subOutStateNonStateVars[
		Print["h2"];
	subOutEps[
			Print["h1"];
		subOutPiecewise[eqns]
		]
		]
		]
		]
		]
		]
		]
		]
		]
		]
		]
		]
(*?FreeQ[#,Global`eqvdIf]&*)
(*
subOutConds[eqns_List]:=
Module[{try1=	eqns//.sub1,try2=eqns//.sub2,try3=doSub2[eqns]},
(*	Print["subOutConds:",{try1,try2,try3}];*)
try2
]

*)

subOutConds[eqns_List]:=
Module[{try=doSubOrAnd[eqns]},
try
]

doSubOrAnd[xx_]:=aPeek//@xx

aPeek[xx_]:=
Module[{aVal=xx//.subNow},
(*	Print["aPeek:",{aVal,xx}//FullForm];*)
	aVal]

subNow={cond_Symbol[EquationValDrv[aa_String],
   EquationValDrv[bb_String]]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>bb<>")"],
      aCondPr:(_?onlyCompare[EquationValDrv[_String]|_?noVars,EquationValDrv[_String]|_?noVars]):>
    doACond[aCondPr],
	cond_Symbol[condPrs:(_?onlyCompare[EquationValDrv[_String]|_?noVars,EquationValDrv[_String]|_?noVars])..]/;onlyAndOr[cond]:>
    condPairs[doConds[(List[condPrs]),cond]],
    	cond_Symbol[condPrs:(EquationValDrv[_String])..]/;onlyAndOr[cond]:>
    EquationValDrv[doStrConds[(List[condPrs]),cond]]
}



(*
sub1={(*
		Global`eqvdIf[cond_Symbol/;Or[cond===Or,cond===And][condPrs__/;FreeQ[List@condPrs,Global`eqvdIf[___]]],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]/;FreeQ[List[condPrs],Global`eqvdIf]:>
      EquationValDrv[
    doConds[(List[condPrs]),cond]<>
      	").eqvdIf("<>cc<>","<>dd<>")"]
*)}
*)
sub2={(*
	cond_Symbol[condPrs__?noEvdOrAnd]/;onlyAndOr[cond]:>
    condPairs[doConds[(List[condPrs]),cond]],
    condPairs[condPairs[inner_String],outer_String]:>
    condPairs["("<>inner<>")"<>outer],
	Global`eqvdIf[cond_Symbol[condsStr_String],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]/;onlyAndOr[cond]:>
      EquationValDrv[
    condsStr<>
      	").eqvdIf("<>cc<>","<>dd<>")"],
(*      			Global`eqvdIf[cond_Symbol[condPrs__?noEvdOrAnd],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]/;onlyAndOr[cond]:>
      EquationValDrv[
    doConds[(List[condPrs]),cond]<>
      	").eqvdIf("<>cc<>","<>dd<>")"],*)
   Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],EquationValDrv[bb_String]], 
   	EquationValDrv[cc_String],EquationValDrv[dd_String]]/;onlyCompare[cond]:>
      	      EquationValDrv[aa<>rightCondStr[cond]<>bb<>").eqvdIf("<>cc<>","<>dd<>")"]/;onlyCompare[cond],
   Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],bb_?noVars], 
   	EquationValDrv[cc_String],EquationValDrv[dd_String]]/;onlyCompare[cond]:>
      	      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>dd<>")"]/;onlyCompare[cond]
*)}
(*
sub2a={   Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],bb_], 
   	EquationValDrv[cc_String],EquationValDrv[dd_String]]:>
      	      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>dd<>")"]/;onlyCompare[cond]}

sub3={
		Global`eqvdIf[cond_Symbol[condPrs__?noEvdOrAnd],___]:>Global`eqvdIf["ahitsub3"]
}
sub4={		Global`eqvdIf[cond_Symbol[condPrs__],___]:>Global`eqvdIf["ahitsub3"]
}
*)(*

doSub2[xx_]:=aPeek//@xx
aPeek[xx_]:=
Module[{aVal=xx//.sub2,fir=xx//.sub3,lst=xx//.sub4},
	Print["aPeek:",{aVal,fir,xx}//FullForm];
	aVal]
	*)
noEvdOrAnd[xx_]:=Module[{theVal=And[(*noCompare[xx],*)FreeQ[xx,Global`eqvdIf|Or|And|Plus|Times]]},(*Print["noEqvdOrAnd:",xx,theVal];*)
theVal]
onlyAndOr[xx_]:=Or[xx===And,xx===Or]
onlyCompare[xx_]:=MemberQ[{Greater,GreaterEqual,Equal,LessEqual,Less},xx]
noCompare[xx_]:=Module[{theVal=FreeQ[xx,Greater|GreaterEqual|Equal|LessEqual|Less]},(*Print["noCompare:",xx,theVal];*)
	theVal]

condSubs={
(*	cond_[aa:(__?noVars),bb:(__?noVars)]:>ToString[CForm[aa]]<>rightCondStr[cond]<>ToString[CForm[bb]]<>")",  need to generate logical value same for whole grid*)
	cond_[aa:(__?noVars),EquationValDrv[bb_String]]:>bb<>invertCondStr[cond]<>ToString[CForm[aa]]<>")",
	cond_[EquationValDrv[aa_String],bb:(__?noVars)]:>aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>")",
	cond_[EquationValDrv[aa_String],EquationValDrv[bb_String]]:>aa<>rightCondStr[cond]<>bb<>")"
}
toStringButNotHead[yy_]:=yy//.condSubs

doConds[theConds_?noEvdOrAnd,theLogic_Symbol]:=Module[{condsList=theConds,lStr=logicStr[theLogic]},
	Print["doConds:",logicStr[theLogic],lStr,condsList];
With[{nvStr=(*#/.xx_/;And[Head[xx]=!=String,ProjectionInterface`Private`noVars[xx]]:>*)toStringButNotHead/@condsList},
	Print["nvStr=",nvStr,toStringButNotHead/@condsList];
With[{theRes=Fold[((#1/.{condPairs[str_String],{condPairs[str_String]}}:>str)<>logicStr[theLogic]<>(#2//.condSubs)<>")")&,
	(nvStr[[1]]//.condSubs)/.{condPairs[str_String],{condPairs[str_String]}}:>str,Drop[nvStr,1]]},
(*	Print["doConds:theRes=",theRes];*)
	theRes]]]

doACond[aCond:(_?onlyCompare[lft_,rgt_])]:=EquationValDrv[aCond//.condSubs]

doStrConds[theConds_?noEvdOrAnd,theLogic_Symbol]:=Module[{condsList=theConds,lStr=logicStr[theLogic]},
(*	Print["doStrConds:",logicStr[theLogic],lStr,condsList];*)
With[{theRes=Fold[(#1<>logicStr[theLogic]<>(#2[[1]])<>")")&,
	condsList[[1,1]],Drop[condsList,1]]},
(*	Print["doStrConds:theRes=",theRes];*)
	theRes]]


logicStr[cond_Symbol]:=Module[{},(*Print["here",cond];*)
	Switch[cond,Or,".or(",And,".and("]]

rightCondStr[cond_Symbol]:=Switch[cond,GreaterEqual,".ge(",Greater,".gt(",LessEqual,".le(",Less,".lt("]
invertCondStr[cond_Symbol]:=Switch[cond,GreaterEqual,".lt(",Greater,".le(",LessEqual,".gt(",Less,".ge("]

subOutEqvdIf[eqns_List]:=eqns/.{
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],
   EquationValDrv[bb_String]],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>bb<>").eqvdIf("<>cc<>","<>dd<>")"],
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],bb:(__?noVars)],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>dd<>")"],
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],
   bb:(__?noVars)],
   cc:(__?noVars),
   EquationValDrv[dd_String]]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>ToString[CForm[cc]]<>","<>dd<>")"],
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],
   bb:(__?noVars)],
   EquationValDrv[cc_String],
   dd:(__?noVars)]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],
   bb:(__?noVars)],
   EquationValDrv[cc_String],
   EquationValDrv[dd:(__?noVars)]]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[cond_Symbol[EquationValDrv[aa_String],
   bb:(__?noVars)],
   cc:(__?noVars),
   dd:(__?noVars)]/;onlyCompare[cond]:>
      EquationValDrv[aa<>rightCondStr[cond]<>ToString[CForm[bb]]<>").eqvdIf("<>ToString[CForm[cc]]<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[EquationValDrv[cStr_String],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]:>
      EquationValDrv["("<>cStr<>").eqvdIf("<>cc<>","<>dd<>")"],
Global`eqvdIf[EquationValDrv[cStr_String],
   cc_?noVars,
   EquationValDrv[dd_String]]:>
      EquationValDrv["("<>cStr<>").eqvdIf("<>ToString[CForm[cc]]<>","<>dd<>")"],
Global`eqvdIf[EquationValDrv[cStr_String],
   EquationValDrv[cc_String],
   dd_?noVars]:>
      EquationValDrv["("<>cStr<>").eqvdIf("<>cc<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[{condPairs[cStr_String]},
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]:>
      EquationValDrv["("<>cStr<>").eqvdIf("<>cc<>","<>dd<>")"]
}


	
	
makeParseSubs[eqns_List]:=
subOutEqValDrv[
FixedPoint[
subOutEqvdIf[
subOutConds[
subOutDoInt[
subOutErf[
subOutPower[
subOutPlus[
subOutTimes[
subOutRational[
	Identity[
subOutStateNonStateVars[
	subOutEps[
		subOutPiecewise[#]]]]]]]]]]]]&,eqns]]
	
(*
makeParseSubs[eqns_List]:=
Module[{},
	Print["debugging makeParseSubs"];
FixedPoint[
subOutEqvdIf[
subOutDoInt[
subOutErf[
subOutPower[
subOutPlus[
	subOutTimes[
subOutStateNonStateVars[
	subOutEps[#]]]]]]]]&,eqns]]
		
*)
subOutEqValDrv[eqns_List]:=StringReplace[eqns/.{EquationValDrv[xx_String]:>xx,
Power->Global`pow},"Power"->"Global_pow"]


subOutEps[eqns_List]:=eqns/.
ProtectedSymbols`eps[xx_][ProtectedSymbols`t]:>EquationValDrv[ToString[xx]<>"$Shock$tm1"]

subOutStateNonStateVars[eqns_List]:=eqns/.{
aSymbol_Symbol[ProtectedSymbols`t+1]:>EquationValDrv[ToString[aSymbol]<>"$tp1"],(*was global`*)
aSymbol_Symbol[ProtectedSymbols`t]:> EquationValDrv[ToString[aSymbol]<>"$t"],
aSymbol_Symbol[ProtectedSymbols`t-1]:> EquationValDrv[ToString[aSymbol]<>"$tm1"]
}

subOutExp[eqns_List]:=eqns/.
E^EquationValDrv[xx_String]:> EquationValDrv[xx<>".exp()"]

(*
subOutPlusTimes[eqns_List]:=eqns//.{
Plus[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> 
   Plus[EquationValDrv[xx<>".plus("<>yy<>")"],zz],
Plus[EquationValDrv[xx_String],yy_?noVars,zz___]:> 
   Plus[EquationValDrv[xx<>".plus("<>ToString[yy]<>")"],zz],
Times[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> 
   Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
Times[EquationValDrv[xx_String],yy_?noVars,zz___]:> 
   Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
Times[Power[xx:(__?noVars),-1],EquationValDrv[yy_String]]:>
   EquationValDrv[yy<>".divide("<>ToString[CForm[xx]]<>")"]
}

*)
suboutNoVars[eqns_]:=eqns//.xx_?noVars:>EquationValDrv[ToString[CForm[xx]]]

subOutPlus[eqns_List]:=eqns//.{
Plus[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> 
   Plus[EquationValDrv[xx<>".plus("<>yy<>")"],zz],
Plus[EquationValDrv[xx_String],yy_?noVars,zz___]:> 
   Plus[EquationValDrv[xx<>".plus("<>ToString[CForm[yy]]<>")"],zz],
Plus[EquationValDrv[xx_String],yy_?noVars,zz___]:> 
   Plus[EquationValDrv[xx<>".plus("<>ToString[CForm[yy]]<>")"],zz]
}

subOutTimes[eqns_List]:=eqns//.{
Times[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> 
   Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
Times[EquationValDrv[xx_String],yy_?noVars,zz___]:> 
   Times[EquationValDrv[xx<>".times("<>ToString[CForm[yy]]<>")"],zz],
Times[EquationValDrv[xx_String],yy:(__?noVars),zz___]:> 
   Times[EquationValDrv[xx<>".times("<>ToString[CForm[yy]]<>")"],zz],(*
Times[xx:(__?noVars),yy:(__?noVars),zz___]:> 
   Times[ToString[CForm[xx * yy]],zz],*)
Times[Power[xx:(__?noVars),-1],EquationValDrv[yy_String]]:>
   EquationValDrv[yy<>".divide("<>ToString[CForm[xx]]<>")"]
}

subOutPower[eqns_List]:=eqns//.{
Power[EquationValDrv[xx_String],yy_?noVars]:> 
   EquationValDrv[xx<>".pow("<>ToString[CForm[yy]]<>")"],
Power[EquationValDrv[xx_String],yy:(__?noVars)]:> EquationValDrv[xx<>".pow("<>ToString[CForm[yy]]<>")"],
Power[yy:(__?noVars),EquationValDrv[xx_String]]:> EquationValDrv[xx<>".powValBase("<>ToString[CForm[yy]]<>")"]
}

subOutUniqueEqnName[eqns_List]:=
{yy___,EquationValDrv[xx_],zz___}:> {yy,ToString[Unique["eqn"]]<>"="<>xx,zz};

subOutDoInt[eqns_List]:=eqns/.
doInt[xx__]:>xx

subOutTheDeriv[eqns_List]:={
ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t+1],	bSymbol_Symbol[ProtectedSymbols`t]]:> 
   EquationValDrv[ToString[aSymbol]<>"$tp1$Drv$"<>ToString[bSymbol]<>"$t"],
ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t],bSymbol_Symbol[ProtectedSymbols`t-1]]:> 
   EquationValDrv[ToString[aSymbol]<>"$t$Drv$"<>ToString[bSymbol]<>"$tm1"]
}

subOutRational[eqns_List]:=eqns/.Rational[xx_,yy_]:> N[xx/yy]

(*
subOutEqvdIf[eqns_List]:=eqns/.{
Global`eqvdIf[EquationValDrv[aa_String]>=
   EquationValDrv[bb_String],
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]:>
      EquationValDrv[aa<>".ge("<>bb<>").eqvdIf("<>cc<>","<>dd<>")"],
Global`eqvdIf[EquationValDrv[aa_String]>=bb:(__?noVars),
   EquationValDrv[cc_String],
   EquationValDrv[dd_String]]:>
      EquationValDrv[aa<>".ge("<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>dd<>")"],
Global`eqvdIf[EquationValDrv[aa_String]>=
   bb:(__?noVars),
   cc:(__?noVars),
   EquationValDrv[dd_String]]:>
      EquationValDrv[aa<>".ge("<>ToString[CForm[bb]]<>").eqvdIf("<>ToString[CForm[cc]]<>","<>dd<>")"],
Global`eqvdIf[EquationValDrv[aa_String]>=
   bb:(__?noVars),
   EquationValDrv[cc_String],
   dd:(__?noVars)]:>
      EquationValDrv[aa<>".ge("<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[EquationValDrv[aa_String]>=
   bb:(__?noVars),
   EquationValDrv[cc_String],
   EquationValDrv[dd:(__?noVars)]]:>
      EquationValDrv[aa<>".ge("<>ToString[CForm[bb]]<>").eqvdIf("<>cc<>","<>ToString[CForm[dd]]<>")"],
Global`eqvdIf[EquationValDrv[aa_String]>=
   bb:(__?noVars),
   cc:(__?noVars),
   dd:(__?noVars)]:>
      EquationValDrv[aa<>".ge("<>ToString[CForm[bb]]<>").eqvdIf("<>ToString[CForm[cc]]<>","<>ToString[CForm[dd]]<>")"]
}
*)

doPreEqvdIfPrs[firstPrs:{{_,_}..},elseVal_]:=Module[{theRes},
theRes=Fold[Global`eqvdIf[#2[[2]],#2[[1]],#1]&,elseVal,firstPrs];
theRes]

subOutPiecewise[eqns_List]:=Module[{},
With[{theRes=	
	eqns/.{If->myIf,HoldPattern[Piecewise[firstPrs:{{_,_}..},elseVal_]]:>Evaluate[doPreEqvdIfPrs[firstPrs,elseVal]]}
},
theRes/.myIf->If
]]

(*/.{
	Piecewise[{{xxVal_,xxCond_}},yyVal_]:>Global`eqvdIf[xxCond,xxVal,yyVal]
}
*)
subOutErf[eqns_List]:=eqns/.{
		Erfc[EquationValDrv[xx_String]]:> EquationValDrv[xx<>".erfc()"],
		Erf[EquationValDrv[xx_String]]:> EquationValDrv[xx<>".erf()"]
}

subOutLog[eqns_List]:=eqns/.
		Log[EquationValDrv[xx_String]]:> EquationValDrv[xx<>".log()"]







doEVDSum[Plus[EquationValDrv[ss_String]]] := ss;
doEVDSum[Plus[EquationValDrv[ss_String], rr : EquationValDrv[_String] ..]] := 
 ss <> ".plus(" <> doEVDSum[Plus @@ {rr}] <> ")"

doNoVSum[Plus[ss_]] := ToString[CForm[ss]];
doNoVSum[Plus[ss_, rr : _ ..]] := 
 ss <> ".plus(" <> doNoVSum[Plus @@ {rr}] <> ")"

combineSumStrs[evd_String,noV_String]:=EquationValDrv[evd<>".plus("<>noV<>")"]

Print[" define newcodegensubs"];




noTP1[xx_]:=FreeQ[xx,ProtectedSymbols`t+1]
doInt[{}]:={}
intSub=Plus[ yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, ProtectedSymbols`t + 1]]] &), zz___] :> 
		Plus[doInt[Plus @@ {yy}], zz]
intTermOrNot[anEqn_]:=If[FreeQ[anEqn,ProtectedSymbols`t+1],anEqn,anEqn/.intSub]

futureEqns[theEqns_List]:=With[{futEqns=Select[theEqns, Not[FreeQ[#, ProtectedSymbols`t + 1]] &]},
	With[{allGrouped=intTermOrNot/@Expand[theEqns]},
	With[{intFut=futEqns/.Plus[ yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, ProtectedSymbols`t + 1]]] &), zz___] :> 
		Plus[doInt[Plus @@ {yy}], zz]},
	With[{forInts=Union[Cases[intFut,doInt[___],Infinity]]},With[{targ=Table[Unique["forInt"],{Length[forInts]}]},
		With[{reps=Thread[forInts->targ]},
		{ToString/@targ,First/@forInts,reps,intFut/.reps,allGrouped/.reps}]]]]]]

getGrouped[eqns_List]:=intTermOrNot/@Expand[eqns]
Print["may not need subs here"]

makeIntegSubs[theEqns_List]:=
With[{expandEqns=Expand[theEqns]},
With[{futEqns=Select[expandEqns, Not[FreeQ[#, ProtectedSymbols`t + 1]] &]},
	With[{intFutSplitSum=(futEqns/.Plus[ yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, ProtectedSymbols`t + 1]]] &), zz___] :> 
		Plus[doInt[Plus @@ {yy}], zz])},
		With[{intFut=#/.yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, ProtectedSymbols`t + 1]]] &) :> 
		doInt[yy]& /@intFutSplitSum},
	With[{forInts=Union[Cases[intFut,doInt[___],Infinity]]},
		With[{targ=Table[Unique["forInt"],{Length[forInts]}]},
		intFut(*/.Thread[forInts->targ]*)]]]]]]



doEqCodeSubs[modelName_String,eqns_List,svInfo_List] :=
    Module[ {futureStuff=futureEqns[eqns]},(*Print["future",eqns//InputForm];*)(*Print["fin futureEqns"];*)
        With[ {
            eqNames = Table[ToString[Unique["eqn"]],{Length[eqns]}],EquationValDrvEqns = makeParseSubs[futureStuff[[-1]]]},
            With[ {theAugs = augEqns[eqNames],shkDef=defShocks[eqns],
                theDefs = (StringJoin@@MapThread[("EquationValDrv "<>#1<>"="<>#2(*[[1]]*)<>";\n")&,
                    {eqNames,EquationValDrvEqns}])<>"\n",
                varDefs = makeVarDefs[eqns,svInfo]<>makeShockDefs[eqns]<>
                makeDVarDefs[Union[Cases[eqns,ProtectedSymbols`theDeriv[___],Infinity]]],
                                varDefsAtPt = makeVarDefsAtPt[eqns,svInfo]<>makeShockDefsAtPt[eqns],
            thePrms = theParams[eqns,svInfo]},
                With[ {prmDefs = StringJoin @@(defParamString /@thePrms),
                	getDefs = StringJoin @@(getParamString /@thePrms),
                	setDefs = StringJoin @@(setParamString /@thePrms),
                	updateDefs=makeUpdateParams[thePrms]},
                	With[{theIntDefs=forIntHeader<>forIntBody[futureStuff,eqns,varDefs]<>forIntFooter[eqns],
                		shockEqnDefs=shockAssn[eqns]},
                    writeModel[modelName,modelTop,shkDef,modelNearTop,starCaretToE[varDefs],starCaretToE[varDefsAtPt],starCaretToE[theDefs],theAugs,
                    	getDefs,setDefs,prmDefs,updateDefs,theIntDefs,shockEqnDefs,meFuncDef,useDoShocksDefs,
                    	modelBottom,modelPostParams,modelPostParamsAtPt];
                    (
                   JavaNew[modelName])
                ]
            ]
        ]
    ]]

    
    makeUpdateParams[thePrms_List]:=
    "public void updateParams(double[] paramVec){\n" <>
    MapIndexed[setParamStr,thePrms] <>"\n}"<>
    "\n public double[] getParams(){ double [] paramVec = new "<>ToString[StringForm["double[``];",ToString[Length[thePrms]]]]<>
    MapIndexed[getParamStr,thePrms] <>" return(paramVec);\n}"
    
    setParamStr[aSym_Symbol,{aNum_Integer}]:=
    ToString[StringForm["set$``(paramVec[``]);\n",ToString[aSym],ToString[aNum-1]]]
    getParamStr[aSym_Symbol,{aNum_Integer}]:=
    ToString[StringForm["paramVec[``]=get$``();\n",ToString[aNum-1],ToString[aSym]]]
myVariables[eqns_List] :=
    With[ {allSyms = Cases[eqns,xx_Symbol,Infinity]},
        Complement[allSyms,{Log,Plus,Power.Times,E,Pi}]
    ]



theParams[eqns_List,svInfo_List] :=
    With[ {vars = myVariables[eqns],fsv = Flatten[svInfo]},
        With[ {allSNS = Join[Through[fsv[ProtectedSymbols`t-1]],Through[fsv[ProtectedSymbols`t]],Through[fsv[t+1]]]},
            Select[Complement[vars,Append[allSNS,ProtectedSymbols`t]],(Head[#]== Symbol)&]
        ]
    ]
defParamString[param_Symbol] :=
    ToString[StringForm["public double `1`;\n",ToString[param]]]
setParamString[param_Symbol] :=
    ToString[StringForm["public void set$`1`(double theVal) { `1`=theVal;}\n",ToString[param]]]
getParamString[param_Symbol] :=
    ToString[StringForm[" public double get$`1`() { return(`1`);}\n",ToString[param]]]
starCaretToE[aStr_String] :=
    StringReplace[aStr,{"*^"->"e"}]
augEqns[eqNameList_List] :=
    "EquationValDrv sys="<>((eqNameList//.{yy_,xx_,zz___}:> {yy<>".augSys("<>xx<>")",zz})[[1]])<>";\n"<>"return(sys);}\n"


modelTop = 
"import gov.frb.ma.msu.ProjectionMethodToolsJava.*;\n"<>
"import static java.lang.Math.pow;\n"<>
"public class";
modelNearTop = "extends DoEqns {\n";
meFuncDef=" meFunc(){return(this);}\n"

shocksDefPreamble="\n";
shockAssn[numShocks_Integer]:=shocksDefPreamble <>(StringJoin @@ 
(ToString[StringForm["doShock(`1`,epsVec[`1`]);\n",#]]&/@Range[0,numShocks-1]))<>
	"theStochasticBasis.updateForShocks(meFunc(),newLocs,epsVec);}\n"
	
genShocksLocs[eqns_List]:=With[{sns=getStateNonState[eqns]},
	With[{shks=Sort[getShocksAsVars[eqns]]},
		With[{allOfEm=Join[sns[[1]],shks]},
			Flatten[Position[allOfEm,#]&/@shks]-1]]]
locsAssn[eqns_List]:="int[] newLocs="<>
ToString[genShocksLocs[eqns]]<>";\n"<>
"int [] theSVLocs="<>ToString[betterLocs[eqns]]<>";\n"<>
"theStochasticBasis.setShockVarLocs(theSVLocs);\n"<>
"theStochasticBasis.setTheShockVals(epsVec);\n"


betterLocs[eqns_List]:=With[{oldLocs=genShocksLocs[eqns]+1},
	If[oldLocs=={},{},
With[{allPos=Table[-1,{oldLocs[[-1]]}]},
ReplacePart[allPos,Thread[oldLocs->Range[Length[oldLocs]]-1]]]]]
	
getShocksAsVars[eqns_List]:=With[{shks=getShocks[eqns]},
	shks/.ProtectedSymbols`eps[xx_][ProtectedSymbols`t]:>ToExpression["Global`"<>ToString[xx]<>"$Shock"]]


shocksDefPostamble"theStochasticBasis.updateForShocks(meFunc(),newLocs,epsVec);"
(*
shockFuncDef[numShocks_Integer]:=ToString[StringForm["public double [] theShocks=new double[``];",numShocks]]<>
ToString[StringForm["\n public void setShocks(){theShocks=new double[``];}\n",numShocks]]<>
"\n public void setShocks(double[] epsVec){System.arraycopy(epsVec,0,theShocks,0,epsVec.length);}\n"
*)
useDoShocksDefs="double useShock(final int loc, final StochasticBasis theStochasticBasis){\n"<>
"if(theStochasticBasis.isPerfectForesightQ()) return(0);else {\n"<>
"return(theShocks[loc]);}}\n"<>
"final void doShock(final int loc,final double val){theShocks[loc]=val;}\n"
modelPostParams = "\n
    public EquationValDrv updateValDrv(final StochasticBasis theStochasticBasis) throws ProjectionRuntimeException{";
modelPostParamsAtPt = "\n
    public EquationValDrv updateValDrv(final StochasticBasis theStochasticBasis, double [] aPt) throws ProjectionRuntimeException{";

modelBottom = "
double Global_pow(double base,double exp){
	return(pow(base,exp));}
double Global_pow(double base,int exp){
	return(pow(base,exp));}
}";
writeModel[poo___]:={poo}

(*
                   writeModel[modelName,modelTop,shkDef,modelNearTop,starCaretToE[varDefs],starCaretToE[varDefsAtPt],starCaretToE[theDefs],theAugs,
                    	getDefs,setDefs,prmDefs,updateDefs,theIntDefs,shockEqnDefs,meFuncDef,useDoShocksDefs,
                    	modelBottom,modelPostParams];
*)
writeModel[modelName_String,
	modelTop_String,shkDef_String,modelNearTop_String,
	varDefs_String,varDefsAtPt_String,eqnDefs_String,eqnAugs_String,
	getDefs_String,setDefs_String,prmDefs_String,updateDefs_String,theIntDefs_String,
	shockEqnDefs_String,
	meFuncDef_String,useDoShocksDefs_String,
	modelBottom_String,modelPostParams_String,modelPostParamsAtPt_String] :=
    Module[ {},
        With[ {theFile = modelName<>".java"},
            If[ True(*FileInformation[theFile]== {}*),
                WriteString[theFile,modelTop];
                WriteString[theFile," "<>modelName<>" "];
                WriteString[theFile,modelNearTop];
                WriteString[theFile," "<>modelName<>meFuncDef];
                WriteString[theFile,shkDef];
                WriteString[theFile,prmDefs];
                WriteString[theFile,getDefs];
                WriteString[theFile,setDefs];
                WriteString[theFile,useDoShocksDefs];
                WriteString[theFile,updateDefs];(*from here*)
                WriteString[theFile,modelPostParams];
                WriteString[theFile,varDefs];
                WriteString[theFile,theIntDefs];
                WriteString[theFile,eqnDefs];
                WriteString[theFile,eqnAugs];(*new stuff here*)
                WriteString[theFile,modelPostParamsAtPt];
                WriteString[theFile,varDefsAtPt];
                WriteString[theFile,theIntDefs];
                WriteString[theFile,eqnDefs];
                WriteString[theFile,eqnAugs];
               (* WriteString[theFile,shockEqnDefs];*)
                WriteString[theFile,modelBottom];
                Close[theFile];
                doJavac[modelName]
           ]]]
        
    


makeDVarDefs[{}] :=
    "";
makeDVarDefs[theDVars_List] :=
    With[ {theSVars = DeleteCases[theDVars,ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t],bSymbol_Symbol[ProtectedSymbols`t-1]]]/.ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t+1],bSymbol_Symbol[ProtectedSymbols`t]]:> {ToString[aSymbol],ToString[bSymbol]},theSLagVars = DeleteCases[theDVars,ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t+1],bSymbol_Symbol[ProtectedSymbols`t]]]/.ProtectedSymbols`theDeriv[aSymbol_Symbol[ProtectedSymbols`t],bSymbol_Symbol[ProtectedSymbols`t-1]]:> {ToString[aSymbol],ToString[bSymbol]}},
        "StateVarTime dVT;\n"<>(drvSingle/@theSVars)<>"\n"<>(drvSingleLag/@theSLagVars)<>"\n"
    ]



tm1Vars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[ProtectedSymbols`t-1]->xx,Infinity]]
tVars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[ProtectedSymbols`t]->xx,Infinity]]
tp1Vars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[ProtectedSymbols`t+1]->xx,Infinity]]

eqVDTm1[varName_Symbol] :=
 ToString[StringForm["VT=new StateVarTime(\"`1`\",-1);\n",ToString[varName]]]<>"\n"<>
ToString[StringForm[" final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"

eqStateVDT[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",0); final EquationValDrv `1`$t=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
eqStateVDTp1[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"

eqNonStateVDT[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0); final EquationValDrv `1`$t=NVT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
eqNonStateVDTp1[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=NVT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"


eqVDAtPtTm1[varName_Symbol] :=
 ToString[StringForm["VT=new StateVarTime(\"`1`\",-1);\n",ToString[varName]]]<>"\n"<>
ToString[StringForm[" final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis,aPt);\n",ToString[varName]]]<>"\n"

eqStateVDAtPtT[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",0); final EquationValDrv `1`$t=VT.evalVar(theStochasticBasis,aPt);\n",ToString[varName]]]<>"\n"
eqStateVDAtPtTp1[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=VT.evalVar(theStochasticBasis,aPt);\n",ToString[varName]]]<>"\n"

eqNonStateVDAtPtT[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0); final EquationValDrv `1`$t=NVT.evalVar(theStochasticBasis,aPt);\n",ToString[varName]]]<>"\n"
eqNonStateVDAtPtTp1[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=NVT.evalVar(theStochasticBasis,aPt);\n",ToString[varName]]]<>"\n"


makeVarDefs[theEqns_List,svInfo_List] :=
    With[ {begDef = "\nStateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;\n",
        forTM1 = StringJoin @@ (eqVDTm1/@tm1Vars[theEqns]),
        forT = 
        StringJoin @@ (eqStateVDT/@Intersection[tVars[theEqns],svInfo[[1]]])<>
               StringJoin @@ (eqNonStateVDT/@Intersection[tVars[theEqns],svInfo[[2]]])
        ,
        forTP1 = 
        StringJoin @@ (eqStateVDTp1/@Intersection[tp1Vars[theEqns],svInfo[[1]]])<>
        	        StringJoin @@ (eqNonStateVDTp1/@Intersection[tp1Vars[theEqns],svInfo[[2]]])
        },
        begDef<>forTM1<>forT<>forTP1
    ]
    
makeVarDefsAtPt[theEqns_List,svInfo_List] :=
    With[ {begDef = "\nStateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;\n",
        forTM1 = StringJoin @@ (eqVDAtPtTm1/@tm1Vars[theEqns]),
        forT = 
        StringJoin @@ (eqStateVDAtPtT/@Intersection[tVars[theEqns],svInfo[[1]]])<>
               StringJoin @@ (eqNonStateVDAtPtT/@Intersection[tVars[theEqns],svInfo[[2]]])
        ,
        forTP1 = 
        StringJoin @@ (eqStateVDAtPtTp1/@Intersection[tp1Vars[theEqns],svInfo[[1]]])<>
        	        StringJoin @@ (eqNonStateVDAtPtTp1/@Intersection[tp1Vars[theEqns],svInfo[[2]]])
        },
        begDef<>forTM1<>forT<>forTP1
    ]
Print["why eval of shock t+1?"]
eqVDShocks[varName_Symbol] :=
    	ToString[StringForm["VTS=new ShockVarTime(\"`1`$Shock\",-1); final EquationValDrv `1`$Shock$tm1=VTS.evalVar(theStochasticBasis);\n",
    	ToString[varName]]]<>"\n"
eqVDAtPtShocks[varName_Symbol] :=
    	ToString[StringForm["VTS=new ShockVarTime(\"`1`$Shock\",-1); final EquationValDrv `1`$Shock$tm1=VTS.evalVar(theStochasticBasis,aPt);\n",
    	ToString[varName]]]<>"\n"
makeShockDefs[theEqns_List] :=
    With[ {begDef = "\n",
        forT = StringJoin @@ (eqVDShocks/@shockVars[theEqns])},
 begDef<>forT
    ]
    

makeShockDefsAtPt[theEqns_List] :=
    With[ {begDef = "\n",
        forT = StringJoin @@ (eqVDAtPtShocks/@shockVars[theEqns])},
 begDef<>forT
    ]
    
shockVars[theEqns_List]:=
Union[Cases[theEqns,ProtectedSymbols`eps[xx_Symbol][ProtectedSymbols`t]->xx,\[Infinity]]]

makeFuncForIntegral[intName_String,funcString_String,numShocks_Integer,eqns_List,varDefs_String]:=
Module[{},
(*Print["inmakefunc",{intName,funcString,numShocks,eqns,varDefs}//InputForm];*)
ToString[StringForm["Integrand ``$Func = new Integrand(){\n",ToString[intName]]] <>
ToString[StringForm["public int epsDim(){return(``);};\n",ToString[numShocks]]]<>
ToString[StringForm["public int numVars(){return(0);};\n"]]<>
ToString[StringForm["public int numNodes(){return(0);};\n"]]<>
"public void setShocks(StochasticBasis theStochasticBasis,double[]epsVec){\n"<>
locsAssn[eqns]<>shockAssn[numShocks]<>
ToString[StringForm["public EquationValDrv evaluate(double[]epsVec,StochasticBasis theStochasticBasis)\n"]]<>
ToString[StringForm["throws ProjectionRuntimeException{\n"]]<>
"setShocks(theStochasticBasis,epsVec);"<>(*varDefs<>*)
ToString[StringForm["``;\n",funcString]]<>
ToString[StringForm["return(``);}};\n\n",intName]]<>
ToString[StringForm["EquationValDrv `1`=gh.integrate(`1`$Func,theStochasticBasis);\n\n",intName]]<>"\n"]

forIntFooter[theEqns_List]:=""
forIntHeader="gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theStochasticBasis.getTheGaussHermite();\n\n"
	
defShocks[theEqns_List]:=With[{theShocks=getShocks[theEqns]},
	With[{numShocks=Length[theShocks]},""<>
	ToString[StringForm["double [] theShocks = new double[``];\n",ToString[numShocks]]
	]<>"\n"
	]]
	
forIntBody[theParts_List,theEqns_List,varDefs_String]:=With[{},
	With[{numShocks=Length[shockVars[theEqns]]},
	With[{close=closerInt[theParts[[1]],makeParseSubs[theParts[[2]]]]},
(*Print["inbody",{theParts,codeGenSubs,theEqns,varDefs,close}//InputForm];*)
	(StringJoin @@ MapThread[makeFuncForIntegral[#1,#2,numShocks,theEqns,varDefs]&,{theParts[[1]],close}])
	]]]
	
	forSubsForIntBody[theEqns_List,varDefs_String]:=With[{theParts=futureEqns[theEqns]},
(*Print["subinbody",{theEqns,varDefs,theParts}//InputForm];*)
	With[{numShocks=Length[shockVars[theEqns]]},
	With[{close=closerInt[theParts[[1]],makeParseSubs[theParts[[2]]]]},
	(StringJoin @@ MapThread[makeFuncForIntegral[#1,#2,numShocks,theEqns,varDefs]&,{theParts[[1]],close}])
	]]]
	
	
getShocks[eqns_List]:=Union[Cases[eqns,ProtectedSymbols`eps[__][ProtectedSymbols`t],Infinity]]	
shockAssn[theEqns_List]:=
With[{theShocks=getShocks[theEqns]},
	StringJoin @@MapIndexed[
		ToString[StringForm["EquationValDrv shockEqn`1`=`2`$Shock$t.minus(0);sys=sys.augSys(shockEqn`1`);\n",
			#2[[1]]-1,#1[[0,1]]]]&,
		theShocks]]

	(*
			ToString[StringForm["EquationValDrv shockEqn`1`=`2`$Shock$t.minus(theShocks[`1`]);sys=sys.augSys(shockEqn`1`);\n",]]
	
shockAssn[theEqns_List]:=
With[{theShocks=getShocks[theEqns]},
	StringJoin @@MapIndexed[ToString[StringForm["theShocks[``]=``$Shock$t;\n",#2[[1]]-1,#1[[0,1]]]]&,theShocks]]

	*)
closerInt[someNames_List,someEqns_List]:=
Module[{},(*Print["incloserInt",{someNames,someEqns}];*)
MapThread[("EquationValDrv "<>#1<>"="<>#2<>";\n")&,{someNames,someEqns}]]

closerInt01[someNames_List,someEqns_List]:=
Module[{},(*Print["incloserInt",{someNames,someEqns}];*)
MapThread[("EquationValDrv "<>#1<>"="<>#2<>";\n")&,{someNames,someEqns}]]
	
makeStateVarDefs[theSVars_List] :=
    With[ {begDef = "StateVarTime VT;StateVarTime dVT;\n",sDefs = 
        StringJoin@@(stateTriple/@theSVars)},
        begDef<>sDefs<>"\n"
    ]
	
makeNonStateVarDefs[{theSVars_List,theNSVars_List}] :=
    With[ {begDef = "NonStateVarTime VT;NonStateVarTime dVT;\n",sDefs = 
        StringJoin@@(stateTriple/@theSVars),
        nsDefs = StringJoin@@(nonStateDouble/@theNSVars)},
        begDef<>sDefs<>"\n"<>nsDefs
    ]
	
makeVarDefs[{theSVars_List,theNSVars_List}] :=
  makeStateVarDefs[theSVars]<>makeNonStateVarDefs[theNSVars]
  	
makeVarDefsAtPt[{theSVars_List,theNSVars_List}] :=
  makeStateVarDefsAtPt[theSVars]<>makeNonStateVarDefsAtPt[theNSVars]
  
drvSingle[{varNameA_String,varNameB_String}] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",1);dVT=new StateVarTime(\"`2`\",0); final EquationValDrv `1`$tp1$Drv$`2`$t=NVT.evalDrvVar(theStochasticBasis,dVT);\n",varNameA,varNameB]]<>"\n"

drvSingleLag[{varNameA_String,varNameB_String}] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0);dVT=new StateVarTime(\"`2`\",-1); final EquationValDrv `1`$t$Drv$`2`$tm1=NVT.evalDrvVar(theStochasticBasis,dVT);\n",varNameA,varNameB]]<>"\n"


stateTriple[varName_String] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",-1); final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis);VT=new varTime(\"`1`\",0);EquationValDrv `1`$t = VT.evalVar(theStochasticBasis);VT=new varTime(\"`1`\",1);EquationValDrv `1`$tp1 = VT.evalVar(theStochasticBasis);\n",varName]]<>"\n"



nonStateDouble[varName_String] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0);final EquationValDrv `1`$t = VT.evalVar(theStochasticBasis);NVT=new NonStateVarTime(\"`1`\",1);EquationValDrv `1`$tp1 = VT.evalVar(theStochasticBasis);\n",varName]]<>"\n"


nonStateDoubleAtPt[varName_String] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0);final EquationValDrv `1`$t = VT.evalVar(theStochasticBasis,aPT);NVT=new NonStateVarTime(\"`1`\",1);EquationValDrv `1`$tp1 = VT.evalVar(theStochasticBasis,aPt);\n",varName]]<>"\n"


windowsQ[] := Not[StringFreeQ[$Version, "Windows"]]
doJavac[aFile_String] :=
    With[ {cmdStr = If[ windowsQ[],
    	"\"c:\\Program Files\\Java\\jdk1.7.0_51\\bin\\javac\"" <>" -cp ./;ProjectionMethodToolsJava-0.0.1-SNAPSHOT.jar;Jama-1.0.2-1.0-SNAPSHOT.jar  -target 1.7 "<>"./"<>aFile<>".java",
    	             "/msu/scratch/m1gsa00/jdk1.6.0_02/bin/javac -cp ./:"<>
(Global`$JamaJar)<>":"<>(Global`$ProjJar)<>":"<>Directory[]<>":/msu/scratch/m1gsa00/learnProjection/proto:/msu/scratch/m1gsa00/tryRep/gov/frb/ma/msu/projection/1.0-SNAPSHOT/projection-1.0-SNAPSHOT.jar:/msu/scratch/m1gsa00/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar -target 1.5 "<>aFile<>".java"
                    ]},Print[cmdStr];
        Run[cmdStr]
    ]
    (* "C:/RSMA/Wolfram Research/workbench/jre/bin/javac  @S:/tryBenchWindows/projectionJLinkWin/javaOutput/theArgs "<>"S:/tryBenchWindows/projectionJLinkWin/javaOutput/ -source 1.5 "<>aFile<>".java",
           
    "c:/RSMA/Sun/SDK/jdk/bin/javac @S:/tryBenchWindows/projectionJLinkWin/javaOutput/theArgs -source 1.5"<>"S:/tryBenchWindows/projectionJLinkWin/javaOutput/"<>aFile<>".java"*)
adjustRanges[sns_List,rngs_List]:=rngs(*If[Length[sns[[1]]]==2,
	rngs,
		{rngs[[1]],rngs[[2]],rngs[[1]]}]*)
		
		adjustPowers[sns_List,pws_List]:=pws(*If[Length[sns[[1]]]==2,pws,
		{pws[[1]],pws[[2]],pws[[1]]}]*)

modForEpsilon[snsVarsStrs_List,polyRanges_List,polyPowers_List] :=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",
	snsVarsStrs[[1]],snsVarsStrs[[2]],polyRanges,polyPowers]


getStateNonState[theModel_Symbol] :=
    With[ {eqns = projEquations[theModel]},getStateNonState[eqns]]
     (*   With[ {allVars = DeleteCases[stateVariables[eqns],ProtectedSymbols`eps[_]|ProtectedSymbols`eps[_][_]],theState = laggedState[eqns]},
            {theState,Complement[allVars,theState]}
        ]
    ]*)

getStateNonState[eqns_List] :=
    With[ {},
        With[ {allVars = DeleteCases[stateVariables[eqns],ProtectedSymbols`eps[_]|ProtectedSymbols`eps[_][_]],theState = laggedState[eqns],
        	allShocks=getShocksAsVars[eqns]},
            {theState,Complement[allVars,theState],allShocks}
        ]
    ]
laggedState[eqns_List] :=
    Union[Cases[{eqns},(xx_)[ProtectedSymbols`t-1]->xx,Infinity]]


(*

makeVarSpecList[sVars_List,ssSubs_List,polyRanges_List] :=
    With[ {tmplte = makeVarSpecs[MapThread[makeVarSpec[#1,ToExpression[#1<>"projssxyxyxyxy"]+#2[[1]],ToExpression[#1<>"projss"]+#2[[2]]]&,{sVars,polyRanges}]]},
        tmplte/.ssSubs
    ]
*)(*
makeVarSpecList[sVars_List,polyRanges_List] :=
    With[ {tmplte = makeVarSpecs[MapThread[makeVarSpec[#1,#2[[1]],#2[[2]]]&,{sVars,polyRanges}]]},
        tmplte
    ]
*)
makeVarSpecList[sVars_List,polyRanges_?MatrixQ] :=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec",sVars,polyRanges]
	
	SetAttributes[handleRootExceptions,HoldAll];


handleRootExceptions[rootFunction_] :=
    Block[ {exString,javaException,$JavaExceptionHandler,result,$MessagePrePrint},
        $MessagePrePrint=.;
        $JavaExceptionHandler = (exString = #3)&;
        result = rootFunction;
        javaException = GetJavaException[];
        If[ UnsameQ[javaException,Null],
            ReleaseJavaObject[javaException];
            Message[projection::unknownException,exString];
            Throw[{result,exString},nada],
            result
        ]
    ];


    useModForEpsilon[snsVarsStrs_List,polyRanges_List,polyPowers_List,initWts_List,modEqns_?JavaObjectQ] :=
    With[{pm = JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",
    	snsVarsStrs[[1]],snsVarsStrs[[2]],polyRanges,polyPowers]},
    	pm[collocateProjWts[initWts,modEqns]]]
    
  

linEpsUpdate[oldEpsVec_?VectorQ,newEpsVec_?VectorQ,coeffs_?VectorQ] :=
    With[ {theRatVec = doDiffs[newEpsVec]/doDiffs[oldEpsVec]},
        With[ {theTerms = MapIndexed[patternForK[#2[[1]],theRatVec[[#2[[1]]]],Length[theRatVec]]&,Range[Length[theRatVec]]]},
            Times@@theTerms
        ]
    ]/;2^Length[newEpsVec]== Length[coeffs]

doDiffs[aList_List] :=
    #[[2]]-#[[1]]&/@aList

patternForK[kk_Integer,epsVal_?NumberQ,theDim_Integer] :=
    With[ {lil = Join[Table[1,{2^(kk-1)}],Table[epsVal,{2^(kk-1)}]]},
        Flatten[Table[lil,{2^theDim/(2*(2^(kk-1)))}]]
    ]


qkchk[twoByTwo_List,oldEps_?NumberQ,newEps_?NumberQ] :=
    With[ {theMat = Partition[twoByTwo,3],theRat = newEps/oldEps},
        With[ {theDiag = {1,theRat,theRat*theRat}},
            Flatten[(DiagonalMatrix[theDiag]).theMat.(DiagonalMatrix[theDiag])]
        ]
    ]

(*http://ressim.berlios.de/*)

linCoeffToChebInterval[beta_?NumberQ,theMin_?NumberQ,theMax_?NumberQ] :=
    With[ {fromExp = CoefficientList[fromChebyshevInterval[Global`xx,theMin,theMax],Global`xx]},
        beta*fromExp
    ]

linCoeffToChebInterval[beta_?NumberQ,theSS_?NumberQ,lower_?NumberQ,upper_?NumberQ] :=
    With[ {fromExp = CoefficientList[fromChebyshevInterval[Global`xx,theSS-lower,theSS+upper],Global`xx]},
        beta*fromExp-{beta*theSS,0}
    ]

updateCoeffToNewChebInterval[beta_?NumberQ,{oldlower_?NumberQ,oldupper_?NumberQ},{newlower_?NumberQ,newupper_?NumberQ}] :=
    With[ {oldFromExp = CoefficientList[toChebyshevInterval[Global`xx,-oldlower,oldupper],Global`xx],newFromExp = CoefficientList[fromChebyshevInterval[Global`xx,-newlower,newupper],Global`xx]},
        {oldFromExp,newFromExp}
    ]


tryCmp[{aDeg_Integer,bigDeltaDeg_Integer},{{aLowAA_?NumberQ,aHighAA_?NumberQ},{aLowBigDelta_?NumberQ,aHighBigDelta_?NumberQ}},initWts_?MatrixQ] :=
    With[ {polys = Chop[Expand[Flatten[Outer[Times,phiFunc[Global`xx,aDeg,aLowAA,aHighAA],phiFunc[yy,bigDeltaDeg,aLowBigDelta,aHighBigDelta]]]]]},
        With[ {nodes = Flatten[N[Outer[{#1,#2}&,chebyshevNodes[aDeg+1],chebyshevNodes[bigDeltaDeg+1]]],1]},
            With[ {atNodes = ((polys/.{Global`xx->#[[1]],yy->#[[2]]})&)/@nodes},
                With[ {aggs = initWts.Transpose[{polys}]},
                    {initWts,nodes,polys,atNodes,aggs}
                ]
            ]
        ]
    ]


makePolys[deg_Integer,wts_?MatrixQ] :=
    With[ {polys = phiFunc[Global`xx,deg,aLowKK,aHighKK]},
        With[ {nodes = Flatten[N[chebyshevNodes[aDeg+1]]]},
            With[ {atNodes = ((polys/.{Global`xx->#})&)/@nodes},
                With[ {aggs = Flatten[Apart[wts.Transpose[{polys}]]]},
                    aggs
                ]
            ]
        ]
    ]

(*
nxtWts[prevWts_?MatrixQ] :=
    Module[ {theRes},
        With[ {noRows = Length[prevWts],noCols = Length[prevWts[[1]]]},
            theRes = Table[0.0,{noRows},{noCols+1}];
            theRes[[All,Range[noCols]]] = prevWts
        ];
        theRes
    ]*)
    nxtWts[prevWts_?MatrixQ,oldMaxPows_List,newMaxPows_List] :=
    Module[{newMat=ConstantArray[0,{Length[prevWts],Times @@(1+newMaxPows)}],
    	indices=theIndices[oldMaxPows,newMaxPows]},
   newMat[[All,indices]]=prevWts;newMat]

    allPowers[thePows_List] :=
 With[{rawPows = Range[0, #] & /@ thePows, 
   blkSizes = FoldList[Times, 1, 1 + thePows]},
  With[{better = 
     MapThread[
      expand[#1, #2, blkSizes[[0 - 1]]] &, {rawPows, 
       Drop[blkSizes, -1]}]}, Transpose[better]]]
       
       expand[aList_List, blkSize_Integer, totLngth_Integer] :=
 With[{repeated = Table[#, {blkSize}] & /@ aList}, 
  Flatten[Table[repeated, {totLngth/(blkSize*Length[aList])}]]]
  
    indexPower[thePows_List, sumSoFar_Integer, theMaxPows_List] :=
 If[Length[theMaxPows] == 1, 1 + sumSoFar + thePows[[1]],
  With[{remainingPows = Drop[theMaxPows, -1]},
   indexPower[Drop[thePows, -1], 
    sumSoFar + (thePows[[-1]])*(Times @@ (1 + remainingPows)), 
    remainingPows]]]
(*<<someParameterSubs.mth*)
theIndices[oldMaxPows_List, newMaxPows_List] :=
 With[{allP = allPowers[oldMaxPows]},
  indexPower[#, 0, newMaxPows] & /@ allP]




(**)



onePhiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer,aa_|(aa_|aa_?NumberQ),bb_|(bb_|bb_?NumberQ)] :=
    ChebyshevT[nn,toChebyshevInterval[st,aa,bb]]

onePhiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer] :=
    phiFunc[st,nn,-1,1]



phiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer,aa_|(aa_|aa_?NumberQ),bb_|(bb_|bb_?NumberQ)] :=
    Table[ChebyshevT[ii,toChebyshevInterval[st,aa,bb]],{ii,0,nn}]

phiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer] :=
    phiFunc[st,nn,-1,1]

toChebyshevInterval[aVal_,theMin_,theMax_] :=
    (2*(aVal-theMin)/(theMax-theMin))-1

fromChebyshevInterval[xVal_,theMin_,theMax_] :=
    theMin+((xVal+1)*(theMax-theMin)/2)



chebyshevNodes[nn_Integer] :=
    Table[Cos[((2*kk-1)*Pi)/(2*nn)],{kk,1,nn}]

chebyshevExtrema[nn_Integer] :=
    Table[-Cos[Pi*(jj-1)/(nn-1)],{jj,1,nn}]


(*plain polynomial definitions*)
polyFunc[(st_Symbol|st_?NumberQ),nn_Integer,aa_?NumberQ,bb_?NumberQ] :=
    Table[st^ii,{ii,0,nn}]



(*generic polynomial coeff definitions*)

makeCoeffs[numKPolys_Integer,numThetaPolys_Integer] :=
    makeCoeffs[numKPolys,numThetaPolys] = genVarNames["polyWts",numThetaPolys,numKPolys]


computeProjPoly[aa_?MatrixQ,kPolys_?VectorQ,thetaPolys_?VectorQ] :=
    Plus@@Flatten[Flatten[aa]*Flatten[Outer[Times,kPolys,thetaPolys]]]


(*gauss hermite integration definitions*)
gaussHermiteAbsWts[nn_] :=
    With[ {thePoly = HermiteH[nn,Global`xx]},
        With[ {abscissas = Global`xx/.Solve[N[Reduce[thePoly== 0,{Global`xx}]],Global`xx]},
            With[ {wts = cmpWt[#,nn]&/@abscissas},
                Sort[Transpose[{abscissas,wts}],N[#1[[1]]]<N[#2[[1]]]&]
            ]
        ]
    ]

cmpWt[(absVal_|absVal_?NumberQ),nn_Integer] :=
    (2^(nn-1)*(nn!)*Sqrt[Pi])/(nn^2*(HermiteH[nn-1,absVal]^2))


intEps[aPolyFunc_,ghWts_?MatrixQ] :=
    Plus@@Map[(#[[2]]*aPolyFunc[#[[1]]])&,ghWts]

(**)


getArgs[aFunc_CompiledFunction] :=
    aFunc[[5,1]]
getArgs[aFunc_Function] :=
    If[ Length[aFunc]>1,
        Flatten[{aFunc[[1]]}],
        With[ {slts = Cases[aFunc,Slot[_],Infinity]},
            genVarNames["arbArg",Length[slts]]
        ]
    ]


genVarNames[beg_String,tot_Integer] :=
    Map[Symbol[beg<>ToString[#]]&,Range[tot]]
genVarNames[beg_String,totA_Integer,totB_Integer] :=
    Partition[Map[Symbol[beg<>ToString[#]]&,Range[totA*totB]],totA]

applyListOfFuncsToAnArg[ffs_List, theArg_?myNumberQ] := 
 Through[ffs[theArg]]
applyListOfFuncsToListOfArgs[ffs_List, theArgsList_List] := 
 Map[applyListOfFuncsToAnArg[ffs, #] &, theArgsList]
applyListOfListOfFuncsToListOfListOfArgs[ffsList_List, theArgs_] := 
 MapThread[applyListOfFuncsToListOfArgs, {ffsList, theArgs}]
computeTiZk[thePolys_List, theNodes_List] := 
 Transpose[applyListOfFuncsToListOfArgs[thePolys, theNodes]]
computeAllTiZk[listOfListofPolys_List, listOfListOfNodes_List] := 
 MapThread[computeTiZk, {listOfListofPolys, listOfListOfNodes}]
computeWklTensor[ff_Function, argsForTensor_List] :=
 With[{tensorOfArgs = Outer @@ Join[{List}, argsForTensor]}, 
  Map[ff @@ # &, tensorOfArgs, {Length[Dimensions[tensorOfArgs]] - 1}]]
computeAllIJTProdTensors[theTiZk_List] := 
 With[{firstCut = Outer @@ Join[{doTProd}, theTiZk, {1}]}, firstCut]
doTProd[theTs___] := Outer @@ Prepend[{theTs}, Times]
gaussLobattoZeros[kk_Integer, NN_Integer] := 
 Cos[Pi*(kk + 1/2)/NN] /; 0 <= kk < NN
chebInterpNodes[kk_Integer, 
  mm_Integer] := (-1)*Cos[Pi (2*kk - 1)/(2*mm)] /; 
  And[1 <= kk <= mm, mm > 0]
adjustedChebInterpNodes[kk_Integer, mm_Integer, aa_, bb_] := 
 fromChebyshevInterval[chebInterpNodes[kk, mm], aa, bb] /; 
  And[1 <= kk <= mm, aa < bb]
allChebNodes[nn_Integer] := 
 Table[chebInterpNodes[ii, nn + 1], {ii, nn + 1}] /; nn >= 0
allAdjustedNodes[nn_Integer, aa_?myNumberQ, bb_?myNumberQ] := 
 Table[adjustedChebInterpNodes[ii, nn + 1, aa, bb], {ii, nn + 1}] /; 
  And[nn >= 0, aa < bb]
evalPolyAtPts[thePoly_, thePts_List, 
  theVars_List] := (Function @@ {theVars, thePoly}) @@ # & /@ thePts
makePhiFunctions[thePow_Integer] := 
 With[{raw = phiFunc[xx, thePow]}, (Function @@ {xx, #}) & /@ raw]
myNumberQ[xx_] := NumberQ[N[xx]]
computeDenom[thePows_List] := 
 With[{theVals = allChebNodes /@ thePows, 
   theFuncs = makePhiFunctions /@ thePows}, 
  With[{funcsApplied = 
     MapThread[useFuncs, {theFuncs, theVals}]}, {theVals, theFuncs, 
    funcsApplied, sumSquareApplied /@ funcsApplied}]]
useFunc[func_Function, ptList_List] := func /@ ptList
useFuncs[funcList_List, ptList_List] := 
 useFunc[#, ptList] & /@ funcList
sumSquareApplied[
  listOfLists_List] := ((Plus @@ Function[zz, zz*zz] /@ #)) & /@ 
  listOfLists
computeNumer[theFunc_, thePows_List, theRanges_List] :=
 With[{allTheChebNodesForTensor = allChebNodes /@ thePows,
   allTheAdjustedNodesForTensor = 
    MapThread[
     allAdjustedNodes[#1, #2[[1]], #2[[2]]] &, {thePows, theRanges}], 
   allChebPolysForIJTensor = makePhiFunctions[#] & /@ thePows},
  With[{allFVals = 
     Outer @@ Join[{theFunc}, allTheAdjustedNodesForTensor], 
    toMultInTensor = 
     applyListOfListOfFuncsToListOfListOfArgs[allChebPolysForIJTensor,
       allTheChebNodesForTensor]},
   With[{chebMults = Outer @@ Join[{Times}, toMultInTensor]},
    With[{wklFactors = 
       computeWklTensor[theFunc, allTheAdjustedNodesForTensor], 
      tFactors = 
       computeAllIJTProdTensors[
        computeAllTiZk[allChebPolysForIJTensor, 
         allTheChebNodesForTensor]]},
     {allTheChebNodesForTensor, allTheAdjustedNodesForTensor, 
      wklFactors, tFactors(*,wklFactors.Transpose[tFactors]*)}]]]]
computeChebCoeffs[theFunc_, thePows_List, theRanges_List] := With[{top =
    computeNumer[theFunc, thePows, theRanges], 
   bottom = computeDenom[thePows], 
   tableIters = Table[Unique["ii"], {Length[thePows]}]}, 
  With[{theProds = 
     Map[pFlatten[top[[3]]*#] &, top[[4]], {Length[thePows]}], 
    theDenoms = Outer @@ Prepend[bottom[[-1]], Times]}, 
   theProds/theDenoms]]
pFlatten[xx_List] := Plus @@ Flatten[xx]
(*
doRecurIneqOccEqns[{}]:=
Module[{stateVar,nonStateVar,theShock,polyRange,oldSys,zSubs,discrepSub,initPower,shockPower,
lucaBasis,simp,resZ10$0$0,modClass=Unique["modClass"],modSymb=Unique["modSymb"]},
With[{
thePath=genPath[1]/.
{qtm1->globalqq[ProtectedSymbols`t-1],rtm1->globalrr[ProtectedSymbols`t-1],rutm1->globalru[ProtectedSymbols`t-1],ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
With[{zZap=
(Global`zzz$0$1[ProtectedSymbols`t]/.Flatten[Solve[thePath[[5,1]]==globalrunderBar//numIt,Global`zzz$0$1[ProtectedSymbols`t]]])//Expand},
With[{theEqn={
globalqq[ProtectedSymbols`t]-(thePath[[4,1]]),
globalru[ProtectedSymbols`t]-(thePath[[6,1]]),
Global`discrep[ProtectedSymbols`t]-((thePath[[5,1]]/.Global`zzz$0$1[ProtectedSymbols`t]->0)-globalrunderBar//numIt),
globalrr[ProtectedSymbols`t]-(thePath[[5,1]]),
Global`zzz$0$1[ProtectedSymbols`t]-(Global`eqvdIf[Global`discrep[ProtectedSymbols`t]>=0,0,zZap//Expand]//Expand)}},Print["variables in alphabetic orderr and grouped state then nonstate"];
(*Print["theEqns=",theEqn];*)
theEqn]]]]
*)

newGenSys[{}]:=
With[{numZs=1},
With[{aPath=Flatten @genPath[numZs],forConPos=5+3*Range[0,numZs-1],
zVars=Reverse @Flatten[ProjectionInterface`Private`genZVars[numZs-1,1]],
dVars=Reverse @Flatten[ProjectionInterface`Private`genDiscrepVars[numZs-1,1]]},
With[{sEqns={globalqq[ProtectedSymbols`t],globalrr[ProtectedSymbols`t],globalru[ProtectedSymbols`t]}-aPath[[{4,5,6}]]},
With[{forCon=aPath[[forConPos]]},Join[sEqns,(Flatten @
MapThread[makeDiscrepZPair,{forCon,zVars,dVars}])]/.
	{qtm1->globalqq[ProtectedSymbols`t-1],rutm1->globalru[ProtectedSymbols`t-1],ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}]]]]



newGenSys[zSubNow:{{(_Function)..}}]:=
Module[{numZs=Length[zSubNow]+1},
	With[{aPath=Flatten @genPath[numZs],forConPos=5+3*Range[0,numZs-1],
zVars=Reverse @Flatten[ProjectionInterface`Private`genZVars[numZs-1,1]],
dVars=Reverse @Flatten[ProjectionInterface`Private`genDiscrepVars[numZs-1,1]]},
With[{sEqns={globalqq[ProtectedSymbols`t],globalrr[ProtectedSymbols`t],globalru[ProtectedSymbols`t]}-aPath[[{4,5,6}]]},
With[{forCon=aPath[[forConPos]]},
	With[{bigSys=Join[sEqns,(Flatten @
MapThread[makeDiscrepZPair,{forCon,zVars,dVars}])]/.
	{qtm1->globalqq[ProtectedSymbols`t-1],rutm1->globalru[ProtectedSymbols`t-1],ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
	With[{theVals=
	zSubNow[[-1,-1]][aPath[[4,1]],aPath[[6,1]],0]},Print["aPath=",aPath,zSubNow[[-1,-1]]];
	{theVals}
	]]]]]]




newGenSys[numZs_Integer]:=
With[{aPath=Flatten @genPath[numZs],forConPos=5+3*Range[0,numZs-1],
zVars=Reverse @Flatten[ProjectionInterface`Private`genZVars[numZs-1,1]],
dVars=Reverse @Flatten[ProjectionInterface`Private`genDiscrepVars[numZs-1,1]]},
With[{sEqns={globalqq[ProtectedSymbols`t],globalrr[ProtectedSymbols`t],globalru[ProtectedSymbols`t]}-aPath[[{4,5,6}]]},
With[{forCon=aPath[[forConPos]]},Join[sEqns,(Flatten @
MapThread[makeDiscrepZPair,{forCon,zVars,dVars}])]/.
	{qtm1->globalqq[ProtectedSymbols`t-1],rutm1->globalru[ProtectedSymbols`t-1],ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}]]]

globSSubs={qtm1->globalqq[ProtectedSymbols`t-1],
	rutm1->globalru[ProtectedSymbols`t-1],
	ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]};

genCompSlackSys[numZs_Integer]:=
With[{aPath=Flatten @genPath[numZs],forConPos=5+3*Range[0,numZs-1],
zVars=Reverse @Flatten[ProjectionInterface`Private`genZVars[numZs-1,1]],
dVars=Reverse @Flatten[ProjectionInterface`Private`genDiscrepVars[numZs-1,1]]},
With[{sEqns={globalqq[ProtectedSymbols`t],globalru[ProtectedSymbols`t]}-aPath[[{4,6}]]},
With[{forCons=aPath[[forConPos]],theRRs=Table[Symbol["globalrr"<>ToString[ii]][ProtectedSymbols`t],{ii,numZs}]},
With[{adjCons=theRRs-(applyRREqvdIf/@forCons)},
With[{doMT=(Flatten @MapThread[makeDiscrepZPair,{forCons,zVars,dVars}])},
	Join[sEqns,adjCons,doMT]/.globSSubs]]]]]



applyRREqvdIf[eqn_]:=Global`eqvdIf[eqn>0.02,eqn,0.02]

makeDiscrepEqn[anEqn_,zVar:_Symbol[ProtectedSymbols`t],dVar:_Symbol[ProtectedSymbols`t]]:=
dVar-((anEqn/.zVar->0)-0.02)

makeZapEqnVal[anEqn_,zVar:_Symbol[ProtectedSymbols`t]]:=
zVar/.Flatten[Solve[anEqn==0.02,zVar]]

makeDiscrepZPair[anEqn_,zVar:_Symbol[ProtectedSymbols`t],dVar:_Symbol[ProtectedSymbols`t]]:=
{makeDiscrepEqn[anEqn,zVar,dVar],zVar-Global`eqvdIf[dVar>=0,0,makeZapEqnVal[anEqn,zVar]]}


makeForCheck[sysEqns_List]:=
With[{numZs=((Length[sysEqns]-3)/2)-1},
With[{dVars=Flatten[ProjectionInterface`Private`genDiscrepVars[numZs,1]]},
With[{discEqns=sysEqns[[4+2*Range[0,numZs]]],
zEqns=sysEqns[[5+2*Range[0,numZs]]]},
With[{dSolns=Flatten[Solve[Thread[discEqns==0],dVars]]},
zEqns/.dSolns/.Global`eqvdIf->If]]]]


(*
newDoRecurIneqOccEqns[{}]:=
Module[{stateVar,nonStateVar,theShock,polyRange,oldSys,zSubs,discrepSub,initPower,shockPower,
lucaBasis,simp,resZ10$0$0,modClass=Unique["modClass"],modSymb=Unique["modSymb"]},
With[{
thePath=genPath[1]/.
{qtm1->globalqq[ProtectedSymbols`t-1],rtm1->globalrr[ProtectedSymbols`t-1],rutm1->globalru[ProtectedSymbols`t-1],ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
With[{zZap=
(Global`zzz$0$1[ProtectedSymbols`t]/.Flatten[Solve[thePath[[5,1]]==globalrunderBar//numIt,Global`zzz$0$1[ProtectedSymbols`t]]])//Expand},
With[{theEqn={
globalqq[ProtectedSymbols`t]-(thePath[[4,1]]),
globalru[ProtectedSymbols`t]-(thePath[[6,1]]),
Global`discrep[ProtectedSymbols`t]-((thePath[[5,1]]/.Global`zzz$0$1[ProtectedSymbols`t]->0)-globalrunderBar//numIt),
globalrr[ProtectedSymbols`t]-(thePath[[5,1]]),
Global`zzz$0$1[ProtectedSymbols`t]-(Global`eqvdIf[Global`discrep[ProtectedSymbols`t]>=0,0,zZap//Expand]//Expand)}},Print["variables in alphabetic orderr and grouped state then nonstate"];
(*Print["theEqns=",theEqn];*)
theEqn]]]]
*)		
doRecurIneqOcc[{}]:=
Module[{stateVar,nonStateVar,theShock,polyRange,oldSys,zSubs,discrepSub,polys,betterLHS,betterRHS,betterPolys,
lucaBasis,simp,resZ10$0$0,modClass=Unique["modClass"],modSymb=Unique["modSymb"]},
With[{theEqn=doRecurIneqOccEqns[{}]},Print["variables in alphabetic orderr and grouped state then nonstate"];
(*Print["theEqns=",theEqn];*)
newWeightedStochasticBasis[modSymb,theEqn];
{{stateVar, nonStateVar, theShock}, modClass} = 
  GenerateModelCode[modSymb];
polyRange = {{Global`qLow, Global`qHigh}, {globalruLow, globalruHigh}} //. Global`mySubs;
Global`initPower = {0, 0}; Global`shockPower = {0};
lucaBasis = 
  GenerateBasis[stateVar, polyRange //. Global`mySubs, Global`initPower, theShock,
    ProtectedSymbols`theMean //. Global`mySubs, {Global`sigma$uNot} //. Global`mySubs, 
   Global`integOrder //. Global`mySubs, Global`shockPower, nonStateVar];
simp = JavaNew[
   "gov.frb.ma.msu.ProjectionMethodToolsJava.SimpleFindZeroStrategy"];
resZ10$0$0 = 
  ComputeInitialCollocationWeights[lucaBasis, 
   ConstantArray[1, {5, 1}], modClass, simp];
If[resZ10$0$0[isConvergedQ[]],Print["converged 01"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[{1,1,1}]];
If[resZ10$0$0[isConvergedQ[]],Print["converged 02"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[4{1,1,1}]];
If[to$551[isConvergedQ[]],Print["converged 03"],Throw["projection polynomial computation did not converge"]];
{oldSys,zSubs} = Expand[redoCreatePolynomials[modSymb,to$551]]// Chop;
discrepSub=Solve[oldSys[[3]]==0,Global`discrep[ProtectedSymbols`t]]//Flatten;Print[discrepSub];
polys=-1*(PiecewiseExpand/@(Expand[CreatePolynomials[modSymb,to$551]])/.Global`eqvdIf->If)// Chop;
Print["polys=",polys];
(*polys=Expand[((({globalqq[ProtectedSymbols`t],globalru[ProtectedSymbols`t],Global`discrep[ProtectedSymbols`t],globalrr[ProtectedSymbols`t],Global`zzz$0$1[ProtectedSymbols`t]}-oldSys)/.MapThread[#1->#2&,zSubs]))/.Global`eqvdIf->If/.discrepSub]/.
{globalqq[ProtectedSymbols`t-1]->globalqq,globalru[ProtectedSymbols`t-1]->globalru,ProtectedSymbols`eps[globalru][ProtectedSymbols`t]->globalru$Shock};*)(*Print["polys=",polys];repEqns=ReplaceVariables[modSymb,polys,{stateVar,nonStateVar}];*)
(*Print["repEqns=",InputForm[repEqns]];*)(*Print[InputForm[getOrderedOuterString[to$551]]]; shouldn't apply to result*)
{{
Function @@ ({{globalqq,globalru,globalru$Shock},PiecewiseExpand[Expand[polys[[1]]]]}),
Function @@ ({{globalqq,globalru,globalru$Shock},PiecewiseExpand[Expand[polys[[2]]]]}),
Function @@ ({{globalqq,globalru,globalru$Shock},PiecewiseExpand[Expand[polys[[-1]]]]})
}}]]



genXVars[horizons_Integer,numConstr_Integer]:=
genXVars[horizons,numConstr,0]

genXVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{ToExpression["globalqq$"<>theStrModNoT[forTime,ii]],
ToExpression["globalru$"<>theStrModNoT[forTime,ii]]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]

theStrModNoT[forTime_Integer,ii_Integer]:=
ToString[forTime]<>"$"<>ToString[ii]

doRecurIneqOccEqns[zSubNow:{{(_Function)..}..}]:=
Module[{},
With[{numZs=Length[zSubNow]},
With[{zVarNames=Flatten[genZVars[numZs,1]]/.ridTSubs,
xVarsNoT=Drop[Flatten[genXVars[numZs,1],1],0]},(*Print["zvn=",zVarNames];*)
With[{xVars=Through[#[ProtectedSymbols`t]]&/@xVarsNoT},
With[{thePath=genPath[numZs+1]/.
{qtm1->(xVars[[-1,1]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),rtm1->globalrr[ProtectedSymbols`t-1],
rutm1->(xVars[[-1,2]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
With[{(*zZap=(zVarNames[[-1]]/.Solve[thePath[[5,1]]==0.02,zVarNames[[-1]],Reals])//Expand*)},
With[{
xTp1Vals=Join[xVars[[{-1}]],MapThread[{
#2[[1]][#3[[1]][ProtectedSymbols`t],#3[[2]][ProtectedSymbols`t],0],
#2[[2]][#3[[1]][ProtectedSymbols`t],#3[[2]][ProtectedSymbols`t],0]}&,
{Drop[xVars,-2],Drop[zSubNow,-1],Drop[xVarsNoT,-2]}]]
},(*Print["xtp1",{xTp1Vals,zSubNow,xVarsNoT}];*)
With[{
xTp1Eqns=ProjectionInterface`Private`subOutPiecewise[
Thread[Flatten[Drop[Drop[xVars,-1],1]]-Flatten[Drop[xTp1Vals,1]]]],
zSubs=
MapThread[(#1[ProtectedSymbols`t]->
#2[[-1]][#3[[1]],#3[[2]],0])&,
{Drop[zVarNames,-1],zSubNow,xTp1Vals}]
},
With[{zEqns=subOutPiecewise[zSubs/.HoldPattern[xx_->yy_]->xx  -(yy)]},
With[{zZap=(zVarNames[[-1]][ProtectedSymbols`t]/.Flatten[Solve[thePath[[5,1]]==globalrunderBar//numIt,zVarNames[[-1]][ProtectedSymbols`t]]])//Expand},
With[{theEqns=Join[
	({xVars[[-1,1]]-(thePath[[4,1]]),xVars[[-1,2]]-(thePath[[6,1]])}),
{Global`discrep[ProtectedSymbols`t]-((thePath[[5,1]]/.zVarNames[[-1]][ProtectedSymbols`t]->0)-globalrunderBar//numIt)},
zEqns,xTp1Eqns,
{zVarNames[[-1]][ProtectedSymbols`t]-(Global`eqvdIf[Global`discrep[ProtectedSymbols`t]>=0,0,zZap])}]},
theEqns]]]]]]]]]]]/;Length[subNow]==2

doRecurIneqOccEqns[zSubNow:{{(_Function)..}..}]:=
Module[{},
With[{numZs=Length[zSubNow]},
With[{zVarNames=Flatten[genZVars[numZs,1]]/.ridTSubs,
xVarsNoT=Drop[Flatten[genXVars[numZs,1],1],0]},(*Print["zvn=",zVarNames];*)
With[{xVars=Through[#[ProtectedSymbols`t]]&/@xVarsNoT},
With[{thePath=genPath[numZs+1]/.
{qtm1->(xVars[[-1,1]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),rtm1->globalrr[ProtectedSymbols`t-1],
rutm1->(xVars[[-1,2]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
With[{(*zZap=(zVarNames[[-1]]/.Solve[thePath[[5,1]]==0.02,zVarNames[[-1]],Reals])//Expand*)},
With[{
xTp1Vals=Join[xVars[[{-1}]],MapThread[{
#2[[1]][#3[[1]][ProtectedSymbols`t],#3[[2]][ProtectedSymbols`t],0],
#2[[2]][#3[[1]][ProtectedSymbols`t],#3[[2]][ProtectedSymbols`t],0]}&,
{Drop[xVars,-2],Drop[zSubNow,-1],Drop[xVarsNoT,-2]}]]
},(*Print["xtp1",{xTp1Vals,zSubNow,xVarsNoT}];*)
With[{
xTp1Eqns=ProjectionInterface`Private`subOutPiecewise[
Thread[Flatten[Drop[Drop[xVars,-1],1]]-Flatten[Drop[xTp1Vals,1]]]],
zSubs=
MapThread[(#1[ProtectedSymbols`t]->
#2[[-1]][#3[[1]],#3[[2]],0])&,
{Drop[zVarNames,-1],zSubNow,xTp1Vals}]
},
With[{zEqns=subOutPiecewise[zSubs/.HoldPattern[xx_->yy_]->xx  -(yy)]},
With[{zZap=(zVarNames[[-1]][ProtectedSymbols`t]/.Flatten[Solve[thePath[[5,1]]==globalrunderBar//numIt,zVarNames[[-1]][ProtectedSymbols`t]]])//Expand},
With[{theEqns=Join[
	({xVars[[-1,1]]-(thePath[[4,1]]),xVars[[-1,2]]-(thePath[[6,1]])}),
{Global`discrep[ProtectedSymbols`t]-((thePath[[5,1]]/.zVarNames[[-1]][ProtectedSymbols`t]->0)-globalrunderBar//numIt)},
zEqns,xTp1Eqns,
{zVarNames[[-1]][ProtectedSymbols`t]-(Global`eqvdIf[Global`discrep[ProtectedSymbols`t]>=0,0,zZap])}]},
theEqns]]]]]]]]]]]/;Length[zSubNow]==1


doRecurIneqOccEqns[zSubNow:{{(_Function)..}..}]:=
Module[{},
With[{numZs=Length[zSubNow]},
With[{zVarNames=Flatten[genZVars[numZs,1]]/.ridTSubs,
xVarsNoT=Drop[Flatten[genXVars[numZs,1],1],0]},(*Print["zvn=",zVarNames];*)
With[{xVars=Through[#[ProtectedSymbols`t]]&/@xVarsNoT},
With[{thePath=genPath[numZs+1]/.
{qtm1->(xVars[[-1,1]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),rtm1->globalrr[ProtectedSymbols`t-1],
rutm1->(xVars[[-1,2]]/.ProtectedSymbols`t->ProtectedSymbols`t-1),ProtectedSymbols`eps->ProtectedSymbols`eps[globalru][ProtectedSymbols`t]}},
With[{(*zZap=(zVarNames[[-1]]/.Solve[thePath[[5,1]]==0.02,zVarNames[[-1]],Reals])//Expand*)},
With[{
xTp1Vals=MapThread[{
#1[[1]][#2[[1]][ProtectedSymbols`t],#2[[2]][ProtectedSymbols`t],0],
#1[[2]][#2[[1]][ProtectedSymbols`t],#2[[2]][ProtectedSymbols`t],0]}&,
{zSubNow,Drop[xVarsNoT,1]}]
},(*Print["xtp1",{xTp1Vals,zSubNow,xVarsNoT}];*)
With[{
xTp1Eqns=ProjectionInterface`Private`subOutPiecewise[
Thread[Flatten[Drop[Drop[xVars,-1],1]]-Flatten[Drop[xTp1Vals,1]]]],
zSubs=
MapThread[(#1[ProtectedSymbols`t]->
#2[[-1]][#3[[1]],#3[[2]],0])&,
{Drop[zVarNames,-1],zSubNow,xTp1Vals}]
},
With[{zEqns=subOutPiecewise[zSubs/.HoldPattern[xx_->yy_]->xx  -(yy)]},
With[{zZap=(zVarNames[[-1]][ProtectedSymbols`t]/.Flatten[Solve[thePath[[5,1]]==globalrunderBar//numIt,zVarNames[[-1]][ProtectedSymbols`t]]])//Expand},
With[{theEqns=Join[
	({xVars[[-1,1]]-(thePath[[4,1]]),xVars[[-1,2]]-(thePath[[6,1]])}),
{Global`discrep[ProtectedSymbols`t]-((thePath[[5,1]]/.zVarNames[[-1]][ProtectedSymbols`t]->0)-globalrunderBar//numIt)},
zEqns,xTp1Eqns,
{zVarNames[[-1]][ProtectedSymbols`t]-(Global`eqvdIf[Global`discrep[ProtectedSymbols`t]>=0,0,zZap])}]},
theEqns]]]]]]]]]]]/;Length[zSubNow]>1



doRecurIneqOcc[zSubNow:{{(_Function)..}..}]:=
Module[{allZSubs,stateVar,nonStateVar,theShock,polyRange,initPower,shockPower,
lucaBasis,simp,resZ10$0$0,modSymb=Unique["modSymb"],modClass=Unique["modClass"],polys,oldpolys,oldSys,zSubs},
With[{numZs=Length[zSubNow]},
With[{zVarNames=Flatten[genZVars[numZs,1]]/.ridTSubs,
xVarsNoT=Drop[Flatten[genXVars[numZs,1],1],0]},(*Print["zvn=",zVarNames];*)
With[{xVars=Through[#[ProtectedSymbols`t]]&/@xVarsNoT},
With[{theEqns=doRecurIneqOccEqns[zSubNow]
},
(*Print["theEqns=",theEqns//InputForm];*)
newWeightedStochasticBasis[modSymb,(theEqns)];
{{stateVar, nonStateVar, theShock}, modClass} = 
  GenerateModelCode[modSymb];
polyRange = {{Global`qLow, Global`qHigh}, {globalruLow, globalruHigh}} //. Global`mySubs;
(*Print["here",InputForm[{theEqns,zZap,zEqns,zSubs}]];*)
initPower = {0, 0}; shockPower = {0};
lucaBasis = 
  GenerateBasis[stateVar, polyRange //. Global`mySubs, initPower, theShock,
    ProtectedSymbols`theMean //. Global`mySubs, {Global`sigma$uNot} //. Global`mySubs, 
   Global`integOrder //. Global`mySubs, shockPower, nonStateVar];
simp = JavaNew[
   "gov.frb.ma.msu.ProjectionMethodToolsJava.SimpleFindZeroStrategy"];
resZ10$0$0 = 
  ComputeInitialCollocationWeights[lucaBasis, 
   ConstantArray[1, {Length[theEqns], 1}], modClass, simp];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 01"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[{1,1,1}]];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 02"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[2*{1,1,1}]];
If[to$551[isConvergedQ[]]===True,Print["converged 03"],Throw["projection polynomial computation did not converge"]];
oldpolys = Expand[CreatePolynomials[modSymb,to$551]] // Chop;
(*{oldSys,zSubs} = Expand[redoCreatePolynomials[modSymb,to$551]]// Chop;
allZSubs=Flatten[
MapThread[#1->#2&,zSubs]];
discrepSub=Solve[oldSys[[3]]==0,Global`discrep[ProtectedSymbols`t]]//Flatten;Print[discrepSub];
polys=PiecewiseExpand/@Expand[
((({xVars[[-1,1]],xVars[[-1,2]],zVarNames[[-1]]}-oldSys[[{1,2,-1}]])/.allZSubs))](*/.Global`eqvdIf->If*)/.discrepSub/.
{xVarsNoT[[-1,1]][ProtectedSymbols`t-1]->globalqq,xVarsNoT[[-1,2]][ProtectedSymbols`t-1]->globalru,ProtectedSymbols`eps[globalru][ProtectedSymbols`t]->globalru$Shock};(*Print["polys=",polys];repEqns=ReplaceVariables[modSymb,polys,{stateVar,nonStateVar}];*)
(*Print["the Polys=",InputForm[{polys,CreatePolynomials[to$551]}]];*)*)
polys=oldpolys;
Append[zSubNow,
{
Function @@ ({{xVarsNoT[[-1,1]],xVarsNoT[[-1,2]],globalru$Shock},polys[[1]]}//Expand),
Function @@ ({{xVarsNoT[[-1,1]],xVarsNoT[[-1,2]],globalru$Shock},polys[[2]]}//Expand),
Function @@ ({{xVarsNoT[[-1,1]],xVarsNoT[[-1,2]],globalru$Shock},polys[[-1]]}//Expand)
}]]]]]]/;
Length[zSubNow]>0


newDoRecurIneqOcc[numZs_Integer]:=
Module[{modSymb,theEqns,modClass,lucaBasis,simp,to$551},
to$551=genProjResults @@(
{modSymb,theEqns,modClass,lucaBasis,simp}=genProjComponents[numZs]);
genPolyFuncs[modSymb,to$551]
]/;numZs>0

genPolyFuncs[modSymb_Symbol,to$551_?JavaObjectQ]:=
Module[{},
If[to$551[isConvergedQ[]]===True,Print["converged 03"],Throw["projection polynomial computation did not converge"]];
polys = Expand[occBindCreatePolynomials[modSymb,to$551]/.Global`eqvdIf->If]//Chop;
	(Function @@ {{globalqq,globalru,globalru$Shock},#})&/@polys]



genProjResults[modSymb_Symbol,theEqns_List,modClass_?JavaObjectQ,lucaBasis_?JavaObjectQ,simp_?JavaObjectQ]:=
Module[{},
resZ10$0$0 = 
  ComputeInitialCollocationWeights[lucaBasis, 
   ConstantArray[1, {Length[theEqns], 1}], modClass, simp];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 01"],Throw["genProjResults:projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[{1,1,0}]];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 02"],Throw["genProjResults:projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[{4,4,2}]];
If[to$551[isConvergedQ[]]===True,Print["converged 03"],Throw["genProjResults:projection polynomial computation did not converge"]];
to$551
]
genProjComponents[numZs_Integer]:=
Module[{modSymb=Unique["Global`modSymb"],lucaBasis,simp,modClass},
With[{theEqns=genCompSlackSys[numZs]},
newWeightedStochasticBasis[modSymb,(theEqns)];
{{stateVar, nonStateVar, theShock}, modClass} = 
  GenerateModelCode[modSymb];
polyRange = {{Global`qLow, Global`qHigh}, {globalruLow, globalruHigh}} //. Global`mySubs;
(*Print["here",InputForm[{theEqns,zZap,zEqns,zSubs}]];*)
initPower = {0, 0}; shockPower = {0};
lucaBasis = 
  GenerateBasis[stateVar, polyRange //. Global`mySubs, initPower, theShock,
    ProtectedSymbols`theMean //. Global`mySubs, {Global`sigma$uNot} //. Global`mySubs, 
   Global`integOrder //. Global`mySubs, shockPower, nonStateVar];
simp = JavaNew[
   "gov.frb.ma.msu.ProjectionMethodToolsJava.SimpleFindZeroStrategy"];
{modSymb,theEqns,modClass,lucaBasis,simp}]]

fixDoRecurIneqOcc[numZs_Integer]:=
Module[{modSymb=Unique["modSymb"]},
With[{theEqns=newGenSys[numZs]},
(*Print["theEqns=",theEqns//InputForm];*)
newWeightedStochasticBasis[modSymb,(theEqns)];
{{stateVar, nonStateVar, theShock}, modClass} = 
  GenerateModelCode[modSymb];
polyRange = {{Global`qLow, Global`qHigh}, {globalruLow, globalruHigh}} //. Global`mySubs;
(*Print["here",InputForm[{theEqns,zZap,zEqns,zSubs}]];*)
initPower = {0, 0}; shockPower = {0};
lucaBasis = 
  GenerateBasis[stateVar, polyRange //. Global`mySubs, initPower, theShock,
    ProtectedSymbols`theMean //. Global`mySubs, {Global`sigma$uNot} //. Global`mySubs, 
   Global`integOrder //. Global`mySubs, shockPower, nonStateVar];
simp = JavaNew[
   "gov.frb.ma.msu.ProjectionMethodToolsJava.SimpleFindZeroStrategy"];
resZ10$0$0 = 
  ComputeInitialCollocationWeights[lucaBasis, 
   ConstantArray[1, {Length[theEqns], 1}], modClass, simp];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 01"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[{1,1,1}]];
If[resZ10$0$0[isConvergedQ[]]===True,Print["converged 02"],Throw["projection polynomial computation did not converge at first stage"]];
to$551 = resZ10$0$0[toOrder[2*{1,1,1}]];
If[to$551[isConvergedQ[]]===True,Print["converged 03"],Throw["projection polynomial computation did not converge"]];
{modSymb,to$551,resZ10$0$0,simp,lucaBasis,modClass}]]


genValsPath[numNonZeroZs_Integer]:=
With[{xtm1={{qtm1},{rtm1},{rutm1}},
rawFParts=Reverse[((doFPart[phimat,fmat,psiz,#,1,0]+doFEpsPart[phimat,fmat,psieps,#,1,0]) &/@Range[0,numNonZeroZs-1])//numIt]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,psieps]+rawFParts[[1]])//numIt},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,psieps]+#2//numIt)&,
bgn,Drop[rawFParts,1]]]]]





genDiscrepVars[horizons_Integer,numConstr_Integer]:=
genDiscrepVars[horizons,numConstr,0]

genDiscrepVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{ToExpression["Global`discrep$"<>ToString[forTime]<>"$"<>ToString[ii]<>"[ProtectedSymbols`t"<>"]"]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]


doGenEpsVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]

doGenEpsVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{ToExpression["ProtectedSymbols`epsVar$"<>ToString[forTime]<>"$"<>ToString[ii]<>"[ProtectedSymbols`t"<>"]"]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]

genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
(*
notgenZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{ToExpression["Global`zzz$"<>ToString[forTime]<>"$"<>ToString[ii]<>"[ProtectedSymbols`t"<>"]"]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]

*)

genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]/;offset<=0

redoGenZExpVars[horizons_Integer,numConstr_Integer]:=
redoGenZExpVars[horizons,numConstr,0]
(*
redoGenZExpVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{ToExpression["Global`zzzExp$"<>ToString[forTime]<>"$"<>ToString[ii]<>"[ProtectedSymbols`t"<>"]"]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]

*)
occBindCreatePolynomialsLeadsOK[aMod_,results_?JavaObjectQ]:=
With[{origPolys=CreatePolynomials[results],
basis=results[getTheWeightedStochasticBasis[]]},
With[{nonStateVars=gtNonStateVars[basis],
stateVars=gtStateVarsNoShocks[basis]},
With[{lhsrhs=GetLhsRhs[aMod],ifSubs=eqvdIfSubs[aMod]},(*Print["stateSubs=",stateSubs];*)
With[{vNames=ToExpression/@
Join[stateVars,nonStateVars]},
With[{ifPos=Flatten[DeleteCases[{Flatten[Position[vNames,Head[#[[1]]]]][[1]]->#}&/@ifSubs,{_,{}}]]},
	With[{allSubs=Thread[(Through[vNames[ProtectedSymbols`t]])->origPolys]},
With[{nonZSubs=ReplacePart[allSubs,ifPos]},
With[{subbed=((lhsrhs[[1]]/.{globalqq[ProtectedSymbols`t-1]->globalqq,globalru[ProtectedSymbols`t-1]->globalru,ProtectedSymbols`eps[globalru][ProtectedSymbols`t]->globalru$Shock})//.nonZSubs)//Expand},
subbed]]]]]]]]

occBindCreatePolynomials[aMod_,results_?JavaObjectQ]:=
With[{origPolys=CreatePolynomials[results],
basis=results[getTheWeightedStochasticBasis[]]},
With[{nonStateVars=gtNonStateVars[basis],
stateVars=gtStateVarsNoShocks[basis]},
With[{lhsrhs=GetLhsRhs[aMod]},(*Print["stateSubs=",stateSubs];*)
With[{vNames=ToExpression/@
Join[stateVars,nonStateVars]},
With[{allSubs=Thread[(Through[vNames[ProtectedSymbols`t]])->origPolys]},
With[{nonZSubs=Cases[allSubs,HoldPattern[xx_?notStartsZzz$->_]],
zPolySubs=Cases[allSubs,HoldPattern[xx_?startsZzz$->_]]},
With[{realZSubs=(#[[1]]->(((#[[2]]/.{globalqq[ProtectedSymbols`t-1]->globalqq,globalru[ProtectedSymbols`t-1]->globalru,ProtectedSymbols`eps[globalru][ProtectedSymbols`t]->globalru$Shock})/.zPolySubs)/.nonZSubs)&)/@ProjectionInterface`Private`makeRealZSubs[lhsrhs]},
With[{subbed=((lhsrhs[[2]]/.{globalqq[ProtectedSymbols`t-1]->globalqq,globalru[ProtectedSymbols`t-1]->globalru,ProtectedSymbols`eps[globalru][ProtectedSymbols`t]->globalru$Shock})/.nonZSubs/.realZSubs)//Expand},
subbed]]]]]]]]

makeRealZSubs[pairs:{lhs_List,rhs_List}]:=Map[#1[[1]]->#[[2]]&,Cases[Transpose[pairs],{xx_?startsZzz$,_}]]

notStartsZzz$[xx_]:=Not[startsZzz$[xx]]
startsZzz$[aVar_Symbol]:=With[{theStr=ToString[aVar]},StringMatchQ[theStr,"zzz$"~~__]]

startsZzz$[aVar_Symbol[ProtectedSymbols`t]]:=With[{theStr=ToString[aVar]},StringMatchQ[theStr,"zzz$"~~__]]

CreatePolynomials[aMod_,results_?JavaObjectQ]:=
With[{origPolys=CreatePolynomials[results],
basis=results[getTheWeightedStochasticBasis[]]},
With[{nonStateVars=gtNonStateVars[basis],
stateVars=gtStateVarsNoShocks[basis]},
With[{lhsVars=lhsAMod[stateVars,nonStateVars]},
With[{cnstrPolys=GetCnstrnsReplaceVariables[aMod,
origPolys,{stateVars,nonStateVars}],justRHS=lhsVars-projEquations[aMod],
stateSubs=MapThread[#1->#2&,{lhsVars,origPolys}]//Expand},(*Print["stateSubs=",stateSubs];*)
If[cnstrPolys==={{},{}},origPolys,
With[{vNames=ToExpression/@
Join[stateVars,nonStateVars]},
With[{theLHS=cnstrPolys[[1]],ridZSubs=MapThread[#1->#2&,cnstrPolys]},
With[{subPos=Flatten[Position[vNames,Head[#]]&/@theLHS]},
With[{guts=(#/.{xx_,yy_}->xx:>yy)& /@Transpose[{subPos,cnstrPolys[[2]]}]},(*Print["guts=",guts];*)
With[{subbed=(ReplacePart[justRHS,#]&@guts )//.ridZSubs[[{-1,-2}]]},(*Print["sbbed=",subbed];*)(*Print["stateSubs=",stateSubs];*)
With[{theRes=
(*PiecewiseExpand/@*)(((((subbed/.(ProtectedSymbols`eps[xx_][ProtectedSymbols`t]:>ToExpression["Global`"<>ToString[xx]<>"$Shock"])(*/.Global`eqvdIf->If*)))/.
xx_[ProtectedSymbols`t-1]->xx)/.stateSubs)//Expand)//Chop},(*Print["theRes=",theRes];*)
theRes]]]]]]]]]]]




lhsAMod[stateVars_List,nonStateVars_List]:=
		With[{lhs=Through[(ToExpression/@Join[stateVars,nonStateVars])[ProtectedSymbols`t]]},lhs]

redoCreatePolynomials[aMod_,results_?JavaObjectQ]:=
With[{origPolys=projEquations[aMod],
basis=results[getTheWeightedStochasticBasis[]]},
With[{nonStateVars=gtNonStateVars[basis],
stateVars=gtStateVarsNoShocks[basis]},
With[{cnstrPolys=GetCnstrnsReplaceVariables[aMod,
origPolys,{stateVars,nonStateVars}]},
{origPolys,cnstrPolys}]]]



(*

GetCnstrnsReplaceVariables[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[ProtectedSymbols`t] +sgn_.*
Global`eqvdIf[xxR_>=yy_,zz_,ww_]},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[ProtectedSymbols`t],
sgn*Global`eqvdIf[xxR>=yy,zz,ww]}]},
Print["constraints explicitly involving future state or non state not validated in general: augment model with dummy for now to check"];
With[{ageCnstrSubs=GetCnstrnsTp1Subs[theMod,thePolys,{state,nonState}]},
With[{
	lsSubs=makeLaggedStateSubs[state],
	csSubs=makeCurrentStateSubs[thePolys,state],
	cnsSubs=makeCurrentNonStateSubs[thePolys,state,nonState],
	nxtsSubs=makeNextStateSubs[thePolys,state],
	nxtnsSubs=makeNextNonStateSubs[thePolys,state,nonState],
	nxtDrvSubsTp1={}(*makeAllFirstDerivTp1[state,nonState,thePolys]*),
	nxtDrvSubsT=makeAllFirstDerivT[state,nonState,thePolys]},
	{rhsForSubbing[[All,1]],(rhsForSubbing[[All,2]]/.nxtDrvSubsTp1/.nxtDrvSubsT)/.Join[lsSubs,nxtsSubs,nxtnsSubs,csSubs,cnsSubs]}]]]]]

*)
GetLhsRhs[theMod_]:=
With[{eqns=projEquations[theMod],
otherPattern=xxL_Symbol[ProtectedSymbols`t] +yyy__},Print["the function getlhsrhs is not working correctly!!!!!!!!!!!!!!!!!!!!!!!!!!!"];
With[{otherForSubbing=Cases[eqns,otherPattern->(xxL[ProtectedSymbols`t]:>(-1)*(Plus @@{yyy}))]},
				{otherForSubbing[[All,1]],otherForSubbing[[All,2]]}
]]


eqvdIfSubs[theMod_]:=
With[{eqns=projEquations[theMod],
otherPattern=xxL_Symbol[ProtectedSymbols`t]+(yyy__?(Not[FreeQ[#,Global`eqvdIf]]&))},
With[{otherForSubbing=Cases[eqns,otherPattern->(xxL[ProtectedSymbols`t]:>(-1)*(Plus @@{yyy})),Infinity]},
				otherForSubbing
]]



		
GetCnstrnsReplaceVariables[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[ProtectedSymbols`t] +sgn_.*
Global`eqvdIf[cond_Symbol[xxR_,yy_],zz_,ww_],
otherPattern=xxL_Symbol[ProtectedSymbols`t] +yyy__},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[ProtectedSymbols`t],
(-1)*sgn*Global`eqvdIf[cond[xxR,yy],zz,ww]}],
otherForSubbing=Cases[eqns,otherPattern->(xxL[ProtectedSymbols`t]:>(-1)*(Plus @@{yyy}))]},
Print["constraints explicitly involving future state or non state not validated in general: augment model with dummy for now to check"];
				{otherForSubbing[[All,1]],otherForSubbing[[All,2]]}
	(*{rhsForSubbing[[All,1]],(rhsForSubbing[[All,2]]/.nxtDrvSubsTp1/.nxtDrvSubsT)/.
		Join[{}(*lsSubs,nxtsSubs,nxtnsSubs,csSubs,cnsSubs*)]}*)]]]


GetCnstrnsTp1Subs[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[ProtectedSymbols`t]+sgn_.*
Global`eqvdIf[cond_Symbol[xxR_,yy_],zz_,ww_]},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[ProtectedSymbols`t],
sgn*Global`eqvdIf[cond[xxR,yy],zz,ww]}]},
Print["constraints explicitly involving future state or non state not implemented yet: augment model with dummy for now"];
With[{},
makeNextStateSubs[thePolys,state]]]]]




CreateRHSPolynomials[theMod_,thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{lhs=Through[Join[state,nonState][ProtectedSymbols`t]]},
With[{ageCnstrSubs=GetCnstrnsTp1Subs[theMod,thePolys,{state,nonState}],
eqns=projEquations[theMod]/.ProtectedSymbols`eps[xx_][ProtectedSymbols`t]:>ToExpression["Global`"<>ToString[xx]<>"$Shock"],
	lsSubs=makeLaggedStateSubs[state],
	csSubs=makeCurrentStateSubs[thePolys,state],
	cnsSubs=makeCurrentNonStateSubs[thePolys,state,nonState],
	nxtsSubs=makeNextStateSubs[thePolys,state],
	nxtnsSubs=makeNextNonStateSubs[thePolys,state,nonState],
	nxtDrvSubsTp1=makeAllFirstDerivTp1[state,nonState,thePolys],
	nxtDrvSubsT=makeAllFirstDerivT[state,nonState,thePolys]},
Print["need modification to actually compute expected value"];
(*Print[csSubs//ExpandAll,nxtsSubs//ExpandAll];*)
	(((eqns-lhs)/.ageCnstrSubs)/.nxtDrvSubsTp1/.nxtDrvSubsT)/.Flatten[Join[lsSubs,csSubs,cnsSubs,nxtnsSubs,nxtsSubs]]]]]


ReplaceVariables[theMod_,thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{ageCnstrSubs=GetCnstrnsTp1Subs[theMod,thePolys,{state,nonState}],
eqns=projEquations[theMod]/.ProtectedSymbols`eps[xx_][ProtectedSymbols`t]:>ToExpression["Global`"<>ToString[xx]<>"$Shock"],
	lsSubs=makeLaggedStateSubs[state],
	csSubs=makeCurrentStateSubs[thePolys,state],
	cnsSubs=makeCurrentNonStateSubs[thePolys,state,nonState],
	nxtsSubs=makeNextStateSubs[thePolys,state],
	nxtnsSubs=makeNextNonStateSubs[thePolys,state,nonState],
	nxtDrvSubsTp1=makeAllFirstDerivTp1[state,nonState,thePolys],
	nxtDrvSubsT=makeAllFirstDerivT[state,nonState,thePolys]},
Print["need modification to actually compute expected value"];
(*Print[csSubs//ExpandAll,nxtsSubs//ExpandAll];*)
	((eqns/.ageCnstrSubs)/.nxtDrvSubsTp1/.nxtDrvSubsT)/.Flatten[Join[lsSubs,csSubs,cnsSubs,nxtnsSubs,nxtsSubs]]]]

makeAllFirstDerivTp1[state_List,nonState_List,thePolys_List]:=
With[{interact=Flatten[Outer[ProtectedSymbols`theDeriv[#1[ProtectedSymbols`t+1],#2[ProtectedSymbols`t]]&,Join[state,nonState],state]],
	curSubs=Join[makeCurrentStateSubs[thePolys,state],makeCurrentNonStateSubs[thePolys,state,nonState]]},
		With[{interactSubVals=Flatten[Outer[D[#1[[2]],#2]&,curSubs,state]]},
	Thread[interact->(interactSubVals/.(curSubs/.xx_[ProtectedSymbols`t]->xx))]]]

	makeAllFirstDerivT[state_List,nonState_List,thePolys_List]:=
With[{interact=Flatten[Outer[ProtectedSymbols`theDeriv[#1[ProtectedSymbols`t],#2[ProtectedSymbols`t-1]]&,Join[state,nonState],state]],
	curSubs=Join[makeCurrentStateSubs[thePolys,state],makeCurrentNonStateSubs[thePolys,state,nonState]]},
		With[{interactSubVals=Flatten[Outer[D[#1[[2]],#2]&,curSubs,state]]},
	Thread[interact->(interactSubVals)]]]

makeLaggedStateSubs[state_List]:=
With[{lagged=Through[state[ProtectedSymbols`t-1]],theSymbols=ToExpression[state]},
	Thread[lagged->theSymbols]]
	
makeCurrentStateSubs[thePolys_List,state_List]:=
With[{numState=Length[state],current=Through[state[ProtectedSymbols`t]]},
	With[{justState=thePolys[[Range[numState]]]},
	Thread[current->justState]]]

	
makeCurrentNonStateSubs[thePolys_List,
	state_List,nonState_List]:=
With[{numState=Length[state],numNonState=Length[nonState],
	current=Through[nonState[ProtectedSymbols`t]]},
	With[{justNonState=thePolys[[numState+Range[numNonState]]]},
	Thread[current->justNonState]]]	
	

shocksTtoTp1[eqns_]:=With[{shocks=findShocks[eqns]},shocks]

	

makeNextStateSubs[thePolys_List,state_List]:=
	With[{numState=Length[state],nxt=Through[state[ProtectedSymbols`t+1]]},
Print["need to generalize code for makeNextStateSubs"];
With[{partialPolys=thePolys[[Range[numState]]]},
	With[{justState=partialPolys/.Global`uu$Shock->Global`notlookey},
	With[{prep=(thePolys[[Range[numState]]])/.Thread[state->justState]/.Global`uu$Shock->Global`lookey},
Thread[nxt->prep]/.Global`notlookey->Global`uu$Shock]]]]



makeNextNonStateSubs[thePolys_List,state_List,nonState_List]:=
	With[{numState=Length[state],nxt=Through[Join[state,nonState][ProtectedSymbols`t+1]]},
	With[{justState=thePolys[[Range[numState]]]},
	With[{prep=thePolys/.Thread[state->justState]},Thread[nxt->prep]]]]


ridTSubs=xx_[ProtectedSymbols`t]->xx;
genPathNoT[len_Integer]:=genPath[len]/.ridTSubs


End[]

EndPackage[]
(*
Global`lucaSubs = {betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
  rUnderBar -> 2/100, qLow -> -1/2, qHigh -> 1/2, 
  ruLow -> -4*sigma$uNot/(1 - rho$ru), ruHigh ->  4*sigma$uNot/(1 - rho$ru),
   integOrder -> {50}, sigma$uNot ->2/100, theMean -> {0}, rho$ru -> 1/2,
   adj -> 1}; 


Global`mySubs={betap -> 99/100, phip -> 1, rhop -> 1/2, sigmap -> 1, 
  (*rUnderBar -> rub,*) qLow -> -1/2, qHigh -> 1/2, 
  ruLow -> -4*sigma$uNot/(1 - rho$ru), ruHigh ->  4*sigma$uNot/(1 - rho$ru),
   integOrder -> {50}, sigma$uNot ->2/100, theMean -> {0}, rho$ru -> 1/2,
   adj -> 1}



*)
(*
numIt[xx_]:=xx//.lucaSubs//Expand//Chop;Print["ProjectionInterface: taking //N out of numIt"];*)


myN[xx_]:=(N[xx/.{t-1->$tm1,t+1->$tp1}])/.{$tm1->t-1,$tp1->t+1}

Print["done reading ProjectionInterface"]
