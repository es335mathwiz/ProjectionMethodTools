(* Mathematica Package *)
(*
BeginPackage["OccasionallyBindingConstraints`", {"JLink`","ProjectionInterface`"}]
(* Exported symbols added here with SymbolName::usage *)  
GetCnstrnsReplaceVariables::usage="GetCnstrnsReplaceVariables[theMod_,thePolys_List,{stateStr_List,nonStateStr_List},theLocs_?ArrayQ]"

CreateRHSPolynomials::usage="CreateRHSPolynomials[aMod_,results_?JavaObjectQ]"
Begin["`Private`"] (* Begin Private Context *) 

CreatePolynomials[aMod_,results_?JavaObjectQ]:=
With[{origPolys=CreatePolynomials[results],
basis=results[getTheWeightedStochasticBasis[]]},
With[{nonStateVars=gtNonStateVars[basis],
stateVars=gtStateVarsNoShocks[basis]},
With[{cnstrPolys=GetCnstrnsReplaceVariables[aMod,
origPolys,{stateVars,nonStateVars}]},
If[cnstrPolys==={{},{}},origPolys,
With[{vNames=ToExpression/@
Join[stateVars,nonStateVars]},
With[{theLHS=cnstrPolys[[1]]},
With[{subPos=Flatten[Position[vNames,Head[#]]&/@theLHS]},
With[{guts=(#/.{xx_,yy_}->xx:>yy)& /@Transpose[{subPos,cnstrPolys[[2]]}]},
With[{subbed=ReplacePart[origPolys,#]&@guts },(*Print["sbbed=",subbed,"guts",guts];*)
PiecewiseExpand/@(subbed/.Global`eqvdIf->If)/.Global`eps[xx_][Global`t]:>ToExpression[ToString[xx]<>"$Shock"]]]]]]]]]]


GetCnstrnsReplaceVariables[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[Global`t] +sgn_.*
Global`eqvdIf[xxR_>=yy_,zz_,ww_]},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[Global`t],
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



GetCnstrnsReplaceVariables[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[Global`t] +sgn_.*
Global`eqvdIf[xxR_>=yy_,zz_,ww_]},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[Global`t],
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


GetCnstrnsTp1Subs[theMod_,
thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{eqns=projEquations[theMod],
thePattern=xxL_Symbol[Global`t]+sgn_.*
Global`eqvdIf[xxR_>=yy_,zz_,ww_]},
With[{rhsForSubbing=Cases[eqns,thePattern->{xxL[Global`t],
sgn*Global`eqvdIf[xxR>=yy,zz,ww]}]},
Print["constraints explicitly involving future state or non state not implemented yet: augment model with dummy for now"];
With[{},
makeNextStateSubs[thePolys,state]]]]]




CreateRHSPolynomials[theMod_,thePolys_List,{stateStr_List,nonStateStr_List}]:=
With[{state=ToExpression[stateStr],nonState=ToExpression[nonStateStr]},
With[{lhs=Through[Join[state,nonState][Global`t]]},
With[{ageCnstrSubs=GetCnstrnsTp1Subs[theMod,thePolys,{state,nonState}],
eqns=projEquations[theMod]/.Global`eps[xx_][Global`t]:>ToExpression[ToString[xx]<>"$Shock"],
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
eqns=projEquations[theMod]/.Global`eps[xx_][Global`t]:>ToExpression[ToString[xx]<>"$Shock"],
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
	

shocksTtoTp1[eqns_]:=With[{shocks=findShocks[eqns]},shocks]

	

makeNextStateSubs[thePolys_List,state_List]:=
	With[{numState=Length[state],nxt=Through[state[Global`t+1]]},
Print["need to generalize code for makeNextStateSubs"];
With[{partialPolys=thePolys[[Range[numState]]]},
	With[{justState=partialPolys/.Global`uu$Shock->Global`notlookey},
	With[{prep=(thePolys[[Range[numState]]])/.Thread[state->justState]/.Global`uu$Shock->Global`lookey},
Thread[nxt->prep]/.Global`notlookey->Global`uu$Shock]]]]



makeNextNonStateSubs[thePolys_List,state_List,nonState_List]:=
	With[{numState=Length[state],nxt=Through[Join[state,nonState][Global`t+1]]},
	With[{justState=thePolys[[Range[numState]]]},
	With[{prep=thePolys/.Thread[state->justState]},Thread[nxt->prep]]]]


End[] (* End Private Context *)

EndPackage[]

*)

Print["OccasionallyBindingConstraints.m loaded, but contributes nothing  code moved to ProjectionInterface.m for now"]
