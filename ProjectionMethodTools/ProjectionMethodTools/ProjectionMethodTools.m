(* Mathematica Package *)

(* Created by the Wolfram Workbench Jun 16, 2013 *)

BeginPackage["ProjectionMethodTools`", {"JLink`"}]
(* Exported symbols added here with SymbolName::usage *) 




(* Mathematica Package *)
(*ToDo make sure rationals mapped to numbers beofre java sees them otherwise obscure error MLGet out of sequence
*)

(* Exported symbols added here with SymbolName::usage *)  
(* Exported symbols added here with SymbolName::usage *) 

applyNew::usage="applyNew[phiNow_?NumberQ,theOrders_List,newModEqns_?JavaObjectQ]\n for AKY paper. 'homotopy' for computing phi"
GenerateBasis::usage="GenerateBasis[{stateVars_List,nonStateVars_List},
	initRanges_?MatrixQ,initPowers_List]\n generates a WeightedStochasticBasis object for state and non state variables and polynomial ranges and powers"
FindMaxError::usage="FindMaxError[polys_List,varRanges_List]\n uses NMaximize to find largest absolute value of functions over the given range\n see ReplaceVariables for subbing system of equation with chebyshev polynomials"
ReplaceVariables::usage="ReplaceVariables[theMod_,thePolys_List,{state_List,nonState_List}]\n substitutes chebyshev polynomials for the state and non state variables in the model equations"
CreatePolynomials::usage="CreatePolynomials[results_?JavaObjectQ]\n Pure mathematica function that uses ProjectionMethodResults to generate a list of the tensor product of the chebyshev polynomials in 'ProjectionMethodTools order"
PlotPolynomials::usage="PlotPolynomials[polys_List,varVals_List]"
GenerateModelCode::usage="GenerateModelCode[theModel_Symbol]"
chebyshevNodes::usage = "chebyshevNodes[nn_Integer]\n pure mathematica functions that computes the chebyshev nodes (to infinite precision)"
genPolysFromChebWts::usage = "genPolysFromChebWts[theWts_?MatrixQ,theStateVars_List,theRanges_?MatrixQ,theOrds_?VectorQ]\n Pure mathematica function that generates a list of the tensor product of the chebyshev polynomials in 'ProjectionMethodTools order"
nearestLQ::usage="nearestLQ[theSolnSubs_List,thePhi_?NumberQ,thePoly_,theSubs_List]"
fromChebyshevInterval::usage = "fromChebyshevInterval[xVal_,theMin_,theMax_]"
    theMin+((xVal+1)*(theMax-theMin)/2)
ComputeInitialCollocationWeights::usage=
"ComputeCollocationWeights[basis,initWts,modEqns]"
ComputeCollocationWeightsToOrder::usage=
"ComputeCollocationWeightsToOrder[previousResult]"
toChebyshevInterval::usage = "toChebyshevInterval[aVal_,theMin_,theMax_] "

genModGrid::usage = "genModGrid[theModel_Symbol,{{AALow_?NumberQ,AAHigh_?NumberQ}, {bigDeltaLow_?NumberQ, bigDeltaHigh_?NumberQ}},{AAPow_?NumberQ, bigDeltaPow_?NumberQ}]"
getPiEtcForPhiWider::usage = "getPiEtcForPhiWider[thePhi_?NumberQ,theModel_Symbol,someSubs_List,polyRanges_List,polyPowers_List,modEqns_?JavaObjectQ,theRange_?NumberQ,wtFunc_Symbol] :=
    "
(*benchMarkSubs::usage = "substitutions for benchmark case";*)(* Mathematica package *)
(* Exported symbols added here with SymbolName::usage *) 
Global`xx::usage = "for displaying polynomial";
Global`t::usage = "the time variable";
(*benchMarkSubs::usage = "substitutions for benchmark case";*)
newWeightedStochasticBasis::usage = "associate equations with model name";
projEquations::usage = "model equations associated with symbol";
(*windowsQ::usage="running windows?"*)
phiFunc::usage="phiFunc[st_|(st_Symbol|st_?NumberQ),nn_Integer,aa_|(aa_|aa_?NumberQ),bb_|(bb_|bb_?NumberQ)]" 
computeChebCoeffs::usage="computeChebCoeffs[theFunc_, thePows_List, theRanges_List]"

doJavac::usage = "doJavac[fName+String] compiles the ProjectionMethodTools model (expects it to be a subclass of DoEqns "
Print["temp export of kronProd"]
kronProd::usage = "kronProd[varList_List,rangeList_{rng1,...,rngk},orders_List]  produces kronecker product of chebyshev polynomials in ProjectionMethodTools order"

displayInfo::usage = "displayInfo"


Begin["`Private`"]
(* Implementation of the package *)

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
  Function @@ {Global`thePhi, 
    1 + (Global`kappa*Global`lambda/((1 - Global`beta)*Global`lambda + Global`kappa^2)*
         Global`thePhi/(Global`sigma + Global`chi) //. graphSubs) }

tOrtp1[exprs_] :=
    Union[Select[exprs,Not[And[FreeQ[#,xx_[Global`t+1]],FreeQ[#,xx_[Global`t]]]]&]]
logCnstrns[eqns_List] :=
    (-1)*
        tOrtp1[Cases[eqns,Log[xx_]->xx,Infinity]]
  
 varCnstrns[aProjMod_?JavaObjectQ] :=
     With[ {sVars = 
     	Through[ToExpression[aProjMod[getTheState[]][getStateVariableNames[]]][Global`t]],sVarstp1 = 
     	Through[ToExpression[aProjMod[getTheState[]][getStateVariableNames[]]][Global`t+1]],
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
	
	
kronProd[theStateVars_List,rngs_List,pws_List]:=
Module[{thePolys},
	thePolys=MapThread[phiFunc[#3,#1,#2[[1]],#2[[2]]]&,
		{pws,rngs,theStateVars}];(*Print[thePolys];*)
Flatten[Outer[Times,Sequence@@thePolys]
	]];

	
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
	With[{params={Global`alpha,Global`anEps,Global`beta,Global`chi,Global`rho,Global`rhoG,Global`sigma,Global`tau}},
	With[{target=N[((params/.someSubs)/.Global`tau->(1-phiNow))],away=N[(((params/.Global`beta->$goodBeta)/.someSubs)/.Global`tau->(1-phiNow))]},
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
 
getPiEtcForPhiWider[theRes_?JavaObjectQ,someSubs_List]:=
With[{theRanges=theRes[getRanges[]],thePows=theRes[getOrders[]],thePhi=1-(theRes[getParams[]])[[-1]]},
    With[{thePolys=genPolysFromChebWts[Chop[theRes@resWeights], {Global`theAA,Global`theAA$Shock, Global`theBigDelta,
    	Global`theGG,Global`theGG$Shock}, 
   theRanges, thePows] // Expand}, 
   With[{theSoln = Solve[thePolys[[3]]== Global`theBigDelta,Global`theBigDelta]},
   	With[{
   	                    theDelta=Global`theBigDelta/.nearestLQ[theSoln,thePhi,thePolys,someSubs]},
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
genPolysFromChebWts[theWts_?MatrixQ,theStateVars_List,theRanges_?MatrixQ,theOrds_?VectorQ]:=
Module[{},(*Print[theWts,theStateVars,theRanges,theOrds];*)
Expand[theWts . kronProd[theStateVars,theRanges,theOrds]]
]
genPolysFromChebWts[theState_?JavaObjectQ,theWts_?MatrixQ]:=
							With[{vNames=ToExpression/@(theState[getStateVariableNames[]])},
		With[{grid=theState[getTheGrid[]]},
			With[{theOrds=grid[getTheOrders[]],vSpec=grid[getTheStateVars[]]},
				With[{theMins=vSpec[getMinVals[]],
				theMaxs=vSpec[getMaxVals[]]},
				With[{reOrd=columnMap[grid[generatePolyOrdersForOuterProduct[]],theOrds]},
					Expand[(theWts[[All,reOrd]]) . kronProd[vNames,Transpose[{theMins,theMaxs}],theOrds]]]]]]]
					
genPolysFromChebWts[theResults_?JavaObjectQ]:=
With[{theState=theResults[getTheWeightedStochasticBasis[]][getTheState[]]},
genPolysFromChebWts[theState,theResults[getResWeights[]]]]	

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
targParams = ({Global`alpha, Global`anEps, Global`beta, Global`chi, Global`rho, Global`rhoG, Global`sigma, Global`tau} /. 
     Global`benchmarkSubs) /. Global`tau -> (1 - phiNow),
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




preParseSubs = {(*Global`eps[_][t]->0,*)aSymbol_Symbol[Global`t+1]:> 
	EquationValDrv[ToString[aSymbol]<>"$tp1"],aSymbol_Symbol[Global`t]:> EquationValDrv[ToString[aSymbol]<>"$t"],
	aSymbol_Symbol[Global`t-1]:> EquationValDrv[ToString[aSymbol]<>"$tm1"],
	E^EquationValDrv[xx_String]:> EquationValDrv[xx<>".exp()"],
	Plus[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> Plus[EquationValDrv[xx<>".plus("<>yy<>")"],zz],
	Plus[EquationValDrv[xx_String],yy_?NumberQ,zz___]:> Plus[EquationValDrv[xx<>".plus("<>ToString[yy]<>")"],zz],
	Times[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
	Times[EquationValDrv[xx_String],yy_?NumberQ,zz___]:> Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
	Power[EquationValDrv[xx_String],yy_?NumberQ]:> EquationValDrv[xx<>".pow("<>ToString[yy]<>")"],
		{yy___,EquationValDrv[xx_],zz___}:> {yy,ToString[Unique["eqn"]]<>"="<>xx,zz}};

codeGenSubs = {doInt[xx__]:>xx,(*Global`eps[_][Global`t]->0,*)Global`theDeriv[aSymbol_Symbol[Global`t+1],
	bSymbol_Symbol[Global`t]]:> EquationValDrv[ToString[aSymbol]<>"$tp1$Drv$"<>ToString[bSymbol]<>"$t"],
	Global`theDeriv[aSymbol_Symbol[Global`t],
		bSymbol_Symbol[Global`t-1]]:> EquationValDrv[ToString[aSymbol]<>"$t$Drv$"<>ToString[bSymbol]<>"$tm1"],
		Rational[xx_,yy_]:> N[xx/yy],aSymbol_Symbol[Global`t+1]:> EquationValDrv[ToString[aSymbol]<>"$tp1"],
		aSymbol_Symbol[Global`t]:> EquationValDrv[ToString[aSymbol]<>"$t"],
		aSymbol_Symbol[Global`t-1]:> EquationValDrv[ToString[aSymbol]<>"$tm1"],
		E^EquationValDrv[xx_String]:> EquationValDrv[xx<>".exp()"],
		Plus[EquationValDrv[xx_String],
		EquationValDrv[yy_String],zz___]:> Plus[EquationValDrv[xx<>".plus("<>yy<>")"],zz],
		Plus[EquationValDrv[xx_String],yy:(_?noVars),zz___]:> Plus[EquationValDrv[xx<>".plus("<>ToString[CForm[yy]]<>")"],zz],
(*	need to find way to handle this case for plus and times where constant all alone without dimension information	Plus[xx:(_?noVars),yy:(_?noVars),zz___]:> Plus[EquationValDrv[ToString[CForm[xx]]<>".plus("<>ToString[CForm[yy]]<>")"],zz],*)
	    Times[EquationValDrv[xx_String],EquationValDrv[yy_String],zz___]:> Times[EquationValDrv[xx<>".times("<>ToString[yy]<>")"],zz],
		Times[EquationValDrv[xx_String],yy:(_?noVars),zz___]:> Times[EquationValDrv[xx<>".times("<>ToString[CForm[yy]]<>")"],zz],
		Log[EquationValDrv[xx_String]]:> EquationValDrv[xx<>".log()"],
		Power[EquationValDrv[xx_String],yy:(__?noVars)]:> EquationValDrv[xx<>".pow("<>ToString[CForm[yy]]<>")"],
		Global`eps[xx_][Global`t]:>EquationValDrv[ToString[xx]<>"$Shock$tm1"]};
noVars[aThing___] :=
    Cases[aThing,EquationValDrv[___]|_Symbol[Global`t-1]|_Symbol[t]|_Symbol[Global`t+1]|_?NumberQ,Infinity]==={}

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
    Union[Join[Cases[modelEquations,x_[Global`t_]->0,Infinity],Cases[modelEquations,x_[Global`t+v_]->v,Infinity]]];



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
    Union[Cases[{modelEquations},x_[Global`t]|x_[Global`t+i_]->x,Infinity]]



reformatSNSVars[snsVars_SV] :=
    With[ {justState = DeleteCases[snsVars[[2]],Global`eps[_][_]]/.xx_[_]->xx,
    	noEps = DeleteCases[snsVars[[1]],Global`eps[_]|0]},
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
	
CreatePolynomials[results_?JavaObjectQ]:=
With[{basis=results[getTheWeightedStochasticBasis[]],theWts=results[getResWeights[]]},
	Print["before genPolys in createpolynomials 1"];
	With[{theState=basis[getTheState[]]},
		With[{vNames=ToExpression/@(theState[getStateVariableNames[]]),
			grid=theState[getTheGrid[]]},
			With[{theOrds=grid[getTheOrders[]],vSpec=grid[getTheStateVars[]]},
				With[{
				theMins=vSpec[getMinVals[]],
				theMaxs=vSpec[getMaxVals[]]},Print["before genPolys in createpolynomials"];
					genPolysFromChebWts[theState,Chop[theWts]]]]]]]
					
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

ReplaceVariables[theMod_,thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
	lsSubs=makeLaggedStateSubs[state],
	csSubs=makeCurrentStateSubs[thePolys,state],
	cnsSubs=makeCurrentNonStateSubs[thePolys,state,nonState],
	nxtsSubs=makeNextStateSubs[thePolys,state],
	nxtnsSubs=makeNextNonStateSubs[thePolys,state,nonState],
	nxtDrvSubsTp1=makeAllFirstDerivTp1[state,nonState,thePolys],
	nxtDrvSubsT=makeAllFirstDerivT[state,nonState,thePolys]},
	(eqns/.nxtDrvSubsTp1/.nxtDrvSubsT)/.Flatten[Join[lsSubs,csSubs,cnsSubs,nxtnsSubs]]]]

makeAllFirstDerivTp1[state_List,nonState_List,thePolys_List]:=
With[{interact=Flatten[Outer[Global`theDeriv[#1[Global`t+1],#2[Global`t]]&,Join[state,nonState],state]],
	curSubs=Join[makeCurrentStateSubs[thePolys,state],makeCurrentNonStateSubs[thePolys,state,nonState]]},
		With[{interactSubVals=Flatten[Outer[D[#1[[2]],#2]&,curSubs,state]]},
	Thread[interact->(interactSubVals/.(curSubs/.xx_[Global`t]->xx))]]]

	makeAllFirstDerivT[state_List,nonState_List,thePolys_List]:=
With[{interact=Flatten[Outer[Global`theDeriv[#1[Global`t],#2[Global`t-1]]&,Join[state,nonState],state]],
	curSubs=Join[makeCurrentStateSubs[thePolys,state],makeCurrentNonStateSubs[thePolys,state,nonState]]},
		With[{interactSubVals=Flatten[Outer[D[#1[[2]],#2]&,curSubs,state]]},
	Thread[interact->(interactSubVals)]]]

makeLaggedStateSubs[state_List]:=
With[{lagged=Through[state[Global`t-1]],theSymbols=ToExpression[state]},
	Thread[lagged->theSymbols]]
	
makeCurrentStateSubs[thePolys_List,state_List]:=
With[{numState=Length[state],current=Through[state[Global`t]]},
	With[{justState=thePolys[[Range[numState]]]},
	Thread[current->justState]]]

	
makeCurrentNonStateSubs[thePolys_List,
	state_List,nonState_List]:=
With[{numState=Length[state],numNonState=Length[nonState],
	current=Through[nonState[Global`t]]},
	With[{justNonState=thePolys[[numState+Range[numNonState]]]},
	Thread[current->justNonState]]]	
	

	
(*	

makeNextStateSubs[thePolys_List,state_List]:=
	With[{numState=Length[state],nxt=Through[state[t+1]]},
	With[{justState=thePolys[[Range[numState]]]},
	With[{prep=thePolys/.Thread[state->justState]},Thread[nxt->prep]]]]
*)


makeNextNonStateSubs[thePolys_List,state_List,nonState_List]:=
	With[{numState=Length[state],nxt=Through[Join[state,nonState][Global`t+1]]},
	With[{justState=thePolys[[Range[numState]]]},
	With[{prep=thePolys/.Thread[state->justState]},Thread[nxt->prep]]]]


doPolySub[aPoly_,varVals_List]:=
With[{subs=Cases[varVals,{xx_Symbol,yy_?NumberQ}->(xx->yy)]},
	aPoly/.Flatten[subs]]
getPlotVars[varVals_List]:=
Cases[varVals,{_Symbol,_?NumberQ,_?NumberQ},Infinity]
			
GenerateModelCode[theModel_Symbol] :=
    With[ {eqns = projEquations[theModel],snsVars = getStateNonState[theModel]},
        With[ {eqnsCode = doEqCodeSubs[ToString[theModel],eqns,snsVars]},
            {Map[ToString,snsVars,{-1}],eqnsCode}
            ]
    ]/;And[projLags[theModel]<= 1,projLeads[theModel]<= 1]
    
GenerateBasis[{stateVars_List,nonStateVars_List},
	initRanges_?MatrixQ,initPowers_List]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",stateVars,nonStateVars,
initRanges,initPowers]

GenerateBasis[stateVars_List,stateRanges_?MatrixQ,statePowers_List,
	shockVars_List,shockMeans_?VectorQ,shockStDevs_?VectorQ,intOrders_?VectorQ,shockPowers_List,nonStateVars_List]:=
JavaNew["gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis",
	stateVars,stateRanges,statePowers,shockVars,shockMeans,shockStDevs,intOrders,shockPowers,nonStateVars]


doEqCodeSubs[modelName_String,eqns_List,svInfo_List] :=
    Module[ {futureStuff=futureEqns[eqns]},
        With[ {svInfoReady = Map[ToString,svInfo,{-1}],
            eqNames = Table[ToString[Unique["eqn"]],{Length[eqns]}],EquationValDrvEqns = (futureStuff[[-1]])//.codeGenSubs},
            With[ {theAugs = augEqns[eqNames],shkDef=defShocks[eqns],
                theDefs = (StringJoin@@MapThread[("EquationValDrv "<>#1<>"="<>#2[[1]]<>";\n")&,
                    {eqNames,EquationValDrvEqns}])<>"\n",
                varDefs = makeVarDefs[eqns,svInfo]<>makeShockDefs[eqns]<>
                makeDVarDefs[Union[Cases[eqns,Global`theDeriv[___],Infinity]]],
            thePrms = theParams[eqns,svInfo]},
                With[ {prmDefs = StringJoin @@(defParamString /@thePrms),
                	getDefs = StringJoin @@(getParamString /@thePrms),
                	setDefs = StringJoin @@(setParamString /@thePrms),
                	updateDefs=makeUpdateParams[thePrms]},
                	With[{theIntDefs=forIntHeader<>forIntBody[futureStuff,eqns,varDefs]<>forIntFooter[eqns],
                		shockEqnDefs=shockAssn[eqns]},
                    writeModel[modelName,modelTop,shkDef,modelNearTop,starCaretToE[varDefs],starCaretToE[theDefs],theAugs,
                    	getDefs,setDefs,prmDefs,updateDefs,theIntDefs,shockEqnDefs,meFuncDef,useDoShocksDefs,
                    	modelBottom,modelPostParams];
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
        With[ {allSNS = Join[Through[fsv[t-1]],Through[fsv[t]],Through[fsv[t+1]]]},
            Select[Complement[vars,Append[allSNS,Global`t]],(Head[#]== Symbol)&]
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
    "EquationValDrv sys="<>((eqNameList//.{yy_,xx_,zz___}:> {yy<>".augSys("<>xx<>")",zz})[[1]])<>";\n"


modelTop = "import gov.frb.ma.msu.ProjectionMethodToolsJava.*;\n public class"
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
	shks/.Global`eps[xx_][Global`t]:>ToExpression[ToString[xx]<>"$Shock"]]


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

modelBottom = "return(sys);}}";

writeModel[modelName_String,
	modelTop_String,shkDef_String,modelNearTop_String,
	varDefs_String,eqnDefs_String,eqnAugs_String,
	getDefs_String,setDefs_String,prmDefs_String,updateDefs_String,theIntDefs_String,
	shockEqnDefs_String,
	meFuncDef_String,useDoShocksDefs_String,
	modelBottom_String,modelPostParams_String] :=
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
                WriteString[theFile,updateDefs];
                WriteString[theFile,modelPostParams];
                WriteString[theFile,varDefs];
                WriteString[theFile,theIntDefs];
                WriteString[theFile,eqnDefs];
                WriteString[theFile,eqnAugs];
               (* WriteString[theFile,shockEqnDefs];*)
                WriteString[theFile,modelBottom];
                Close[theFile];
                doJavac[modelName]
            (*,Print[" file "<>theFile<>" already exists. use DeleteFile[\""<>theFile<>"\"]"]*)]
        ]
    ]


makeDVarDefs[{}] :=
    "";
makeDVarDefs[theDVars_List] :=
    With[ {theSVars = DeleteCases[theDVars,Global`theDeriv[aSymbol_Symbol[Global`t],bSymbol_Symbol[Global`t-1]]]/.Global`theDeriv[aSymbol_Symbol[Global`t+1],bSymbol_Symbol[Global`t]]:> {ToString[aSymbol],ToString[bSymbol]},theSLagVars = DeleteCases[theDVars,Global`theDeriv[aSymbol_Symbol[Global`t+1],bSymbol_Symbol[Global`t]]]/.Global`theDeriv[aSymbol_Symbol[Global`t],bSymbol_Symbol[Global`t-1]]:> {ToString[aSymbol],ToString[bSymbol]}},
        "StateVarTime dVT;\n"<>(drvSingle/@theSVars)<>"\n"<>(drvSingleLag/@theSLagVars)<>"\n"
    ]


tm1Vars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[Global`t-1]->xx,Infinity]]
tVars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[Global`t]->xx,Infinity]]
tp1Vars[theEqns_List] :=
    Union[
    Cases[theEqns,xx_Symbol[Global`t+1]->xx,Infinity]]

eqVDTm1[varName_Symbol] :=
 ToString[StringForm["VT=new StateVarTime(\"`1`\",-1);\n",ToString[varName]]]<>"\n"<>
ToString[StringForm[" final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
(*
    ToString[StringForm["VT=new StateVarTime(\"`1`\",-1); final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
*)
eqStateVDT[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",0); final EquationValDrv `1`$t=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
eqStateVDTp1[varName_Symbol] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=VT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"

eqNonStateVDT[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0); final EquationValDrv `1`$t=NVT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"
eqNonStateVDTp1[varName_Symbol] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",1); final EquationValDrv `1`$tp1=NVT.evalVar(theStochasticBasis);\n",ToString[varName]]]<>"\n"

nonStateDouble[varName_String] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0); final EquationValDrv `1`$t=NVT.evalVar(theStochasticBasis);NVT=new NonStateVarTime(\"`1`\",1);final EquationValDrv `1`$tp1 = NVT.evalVar(theStochasticBasis);\n",varName]]<>"\n"
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

eqVDShocks[varName_Symbol] :=
    ToString[StringForm["VTS=new ShockVarTime(\"`1`$Shock\",0); final EquationValDrv `1`$Shock$t=VTS.evalVar(theStochasticBasis);\n",
    	ToString[varName]]]<>"\n"<>
    	ToString[StringForm["VTS=new ShockVarTime(\"`1`$Shock\",-1); final EquationValDrv `1`$Shock$tm1=VTS.evalVar(theStochasticBasis);\n",
    	ToString[varName]]]<>"\n"

futureEqns[theEqns_List]:=With[{futEqns=Select[theEqns, Not[FreeQ[#, Global`t + 1]] &]},
	With[{allGrouped=theEqns/.Plus[xx___, yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, Global`t + 1]]] &), zz___] :> 
		Plus[xx, doInt[Plus @@ {yy}], zz]},
	With[{intFut=futEqns/.Plus[xx___, yy__?(And[FreeQ[#,doInt],Not[FreeQ[#, Global`t + 1]]] &), zz___] :> 
		Plus[xx, doInt[Plus @@ {yy}], zz]},
	With[{forInts=Union[Cases[intFut,doInt[___],Infinity]]},With[{targ=Table[Unique["forInt"],{Length[forInts]}]},
		With[{reps=Thread[forInts->targ]},
		{ToString/@targ,First/@forInts,reps,intFut/.reps,allGrouped/.reps}]]]]]]

makeShockDefs[theEqns_List] :=
    With[ {begDef = "\n",
        forT = StringJoin @@ (eqVDShocks/@shockVars[theEqns])},
 begDef<>forT
    ]
    
shockVars[theEqns_List]:=
Union[Cases[theEqns,Global`eps[xx_Symbol][Global`t]->xx,\[Infinity]]]

makeFuncForIntegral[intName_String,funcString_String,numShocks_Integer,eqns_List,varDefs_String]:=
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
ToString[StringForm["EquationValDrv `1`=gh.integrate(`1`$Func,theStochasticBasis);\n\n",intName]]<>"\n"

forIntFooter[theEqns_List]:=""
forIntHeader="gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theStochasticBasis.getTheGaussHermite();\n\n"
	
defShocks[theEqns_List]:=With[{theShocks=getShocks[theEqns]},
	With[{numShocks=Length[theShocks]},""<>
	ToString[StringForm["double [] theShocks = new double[``];\n",ToString[numShocks]]
	]<>"\n"
	]]
	
forIntBody[theParts_List,theEqns_List,varDefs_String]:=With[{},
	With[{numShocks=Length[shockVars[theEqns]]},
	With[{close=closerInt[theParts[[1]],theParts[[2]]//.codeGenSubs]},
	(StringJoin @@ MapThread[makeFuncForIntegral[#1,#2,numShocks,theEqns,varDefs]&,{theParts[[1]],close}])
	]]]
	
	forSubsForIntBody[theEqns_List,varDefs_String]:=With[{theParts=futureEqns[theEqns]},
	With[{numShocks=Length[shockVars[theEqns]]},
	With[{close=closerInt[theParts[[1]],theParts[[2]]//.codeGenSubs]},
	(StringJoin @@ MapThread[makeFuncForIntegral[#1,#2,numShocks,theEqns,varDefs]&,{theParts[[1]],close}])
	]]]
	
	
getShocks[eqns_List]:=Union[Cases[eqns,Global`eps[__][Global`t],Infinity]]	
shockAssn[theEqns_List]:=
With[{theShocks=getShocks[theEqns]},
	StringJoin @@MapIndexed[
		ToString[StringForm["EquationValDrv shockEqn`1`=`2`$Shock$t.minus(0);sys=sys.augSys(shockEqn`1`);\n",
			#2[[1]]-1,#1[[0,1]]]]&,
		theShocks]]

	(*
			ToString[StringForm["EquationValDrv shockEqn`1`=`2`$Shock$t.minus(theShocks[`1`]);sys=sys.augSys(shockEqn`1`);\n",
	
shockAssn[theEqns_List]:=
With[{theShocks=getShocks[theEqns]},
	StringJoin @@MapIndexed[ToString[StringForm["theShocks[``]=``$Shock$t;\n",#2[[1]]-1,#1[[0,1]]]]&,theShocks]]

	*)
closerInt[someNames_List,someEqns_List]:=
MapThread[("EquationValDrv "<>#1<>"="<>#2[[1]]<>";\n")&,{someNames,someEqns}]

	
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
  
drvSingle[{varNameA_String,varNameB_String}] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",1);dVT=new StateVarTime(\"`2`\",0); final EquationValDrv `1`$tp1$Drv$`2`$t=NVT.evalDrvVar(theStochasticBasis,dVT);\n",varNameA,varNameB]]<>"\n"

drvSingleLag[{varNameA_String,varNameB_String}] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0);dVT=new StateVarTime(\"`2`\",-1); final EquationValDrv `1`$t$Drv$`2`$tm1=NVT.evalDrvVar(theStochasticBasis,dVT);\n",varNameA,varNameB]]<>"\n"


stateTriple[varName_String] :=
    ToString[StringForm["VT=new StateVarTime(\"`1`\",-1); final EquationValDrv `1`$tm1=VT.evalVar(theStochasticBasis);VT=new varTime(\"`1`\",0);EquationValDrv `1`$t = VT.evalVar(theStochasticBasis);VT=new varTime(\"`1`\",1);EquationValDrv `1`$tp1 = VT.evalVar(theStochasticBasis);\n",varName]]<>"\n"


nonStateDouble[varName_String] :=
    ToString[StringForm["NVT=new NonStateVarTime(\"`1`\",0);final EquationValDrv `1`$t = VT.evalVar(theStochasticBasis);NVT=new NonStateVarTime(\"`1`\",1);EquationValDrv `1`$tp1 = VT.evalVar(theStochasticBasis);\n",varName]]<>"\n"


windowsQ[] := Not[StringFreeQ[$Version, "Windows"]]
$repLoc="/msu/res1/Software/mavenRepositories/tryRep";
doJavac[aFile_String] :=
    With[ {cmdStr = If[ windowsQ[],
"c:/RSMA/Sun/SDK/jdk/bin/javac @S:/tryBenchWindows/projectionJLinkWin/javaSource/theArgs  -target 1.5 "<>"./"<>aFile<>".java",
"/msu/scratch/m1gsa00/jdk1.6.0_02/bin/javac -cp ./:"<>
Directory[]<>
":/msu/scratch/m1gsa00/learnProjection/proto:"<>
$repLoc<>
"/gov/frb/ma/msu/ProjectionMethodToolsJava/0.0.1-SNAPSHOT/ProjectionMethodToolsJava-0.0.1-SNAPSHOT.jar:"<>
$repLoc<>
"/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar -target 1.5 "<>
aFile<>
".java"
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
     (*   With[ {allVars = DeleteCases[stateVariables[eqns],Global`eps[_]|Global`eps[_][_]],theState = laggedState[eqns]},
            {theState,Complement[allVars,theState]}
        ]
    ]*)

(*
getShocks[eqns_List] :=
    With[ {},
        With[ {allVars = Cases[stateVariables[eqns],Global`eps[_]|Global`eps[_][_]],},
            {theState,Complement[allVars,theState]}
        ]
    ]

getShocks[theModel_Symbol] :=
    With[ {eqns = projEquations[theModel]},getShocks[eqns]]
     (*   With[ {allVars = DeleteCases[stateVariables[eqns],Global`eps[_]|Global`eps[_][_]],theState = laggedState[eqns]},
            {theState,Complement[allVars,theState]}
        ]
    ]*)*)
getStateNonState[eqns_List] :=
    With[ {},
        With[ {allVars = DeleteCases[stateVariables[eqns],Global`eps[_]|Global`eps[_][_]],theState = laggedState[eqns],
        	allShocks=getShocksAsVars[eqns]},
            {theState,Complement[allVars,theState],allShocks}
        ]
    ]
laggedState[eqns_List] :=
    Union[Cases[{eqns},(xx_)[Global`t-1]->xx,Infinity]]


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


NewtonIterInfoQ[xx_?JavaObjectQ]:=ClassName[xx]=="gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonIterInfo"

NewtonIterSequenceInfoQ[xx_?JavaObjectQ]:=ClassName[xx]=="gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonIterSequenceInfo"

StrategyIterSequenceInfoQ[xx_?JavaObjectQ]:=ClassName[xx]=="gov.frb.ma.msu.ProjectionMethodToolsJava.StrategyIterSequenceInfo"

ProjectionResultsQ[xx_?JavaObjectQ]:=ClassName[xx]=="gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults"

displayInfo[xx_?NewtonIterInfoQ]:=
{{"delta",xx[getDelta[]]},{"fVal",xx[getfVal[]]},{"lambda",xx[getLambda[]]},{"xVal",xx[getXx[]]},{"isConvergedQ",xx[isConvergedQ[]]}}
	
displayInfo[xx_?NewtonIterSequenceInfoQ]:=With[{justNewton=displayInfo /@ xx[toArray[]]},
	{{{"minShrink",xx[getMinShrink[]]},
		{"newtonMethodEpsilon",xx[getNewtonMethodEpsilon[]]},
			{"newtonMethodMaxIterations",xx[getNewtonMethodMaxIterations[]]},
				{"shrinkFactor",xx[getShrinkFactor[]]},
					{"shrinkWhenDone",xx[getShrinkWhenDone[]]}},
		justNewton}]
	
displayInfo[xx_?StrategyIterSequenceInfoQ]:=With[{justStrategy=displayInfo /@ xx[toArray[]]},
	{{{"minShrink",xx[getMinShrink[]]},
		{"newtonMethodEpsilon",xx[getNewtonMethodEpsilon[]]},
		{"newtonMethodMaxIterations",xx[getNewtonMethodMaxIterations[]]},
			{"shrinkFactor",xx[getShrinkFactor[]]},
			{"shrinkWhenDone",xx[getShrinkWhenDone[]]}},
		justStrategy}]

displayInfo[xx_?ProjectionResultsQ]:=displayInfo @ xx[getTheStrategyIters[]]

End[]

EndPackage[]

Print["done reading ProjectionMethodTools"]
