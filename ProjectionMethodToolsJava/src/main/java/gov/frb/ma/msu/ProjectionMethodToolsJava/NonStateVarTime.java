package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class NonStateVarTime extends VarTime {


	   public NonStateVarTime(String aName,int aTime){
			setVarName(aName);
			setVarTime(aTime);
		}
public void setVarTime(int aTime){
	setEvaluationTimeAndType(
VarTimeType.newVarTimeType(aTime,
		VarTimeType.getNONSTATE(),VarTimeType.getNOTDRV()));
				}
	
	
	
	
public EquationValDrv evalVar(StochasticBasis model)throws ProjectionRuntimeException{

	 { 
			  if(model.getTheNonState().hasVarQNSP(this)){
				 
				  return(evalVarNonState(model));}
			  else {
					  {throw new ProjectionRuntimeException("CollocationSolution: var>>"+
							  getVarName()+
					  " not in state or nonState");}}}}	
	
	

 
public EquationValDrv evalVarNonState(StochasticBasis model)throws ProjectionRuntimeException{
EquationValDrv another;
	
		  int varNum = model.getTheNonState().varPositionNSP(getVarName());

		  another=getEvaluationTimeAndType().doValSwitch( model,varNum);

	    
	    return(another);}


/*
EquationValDrv evalVar(StochasticBasis model, double[] evalPt)throws ProjectionRuntimeException{

	 { 
			  if(model.getTheNonState().hasVarQNSP(this)){
				 
				  return(evalVarNonState(model, evalPt));}
			  else {
					  {throw new ProjectionRuntimeException("CollocationSolution: var>>"+
							  getVarName()+
					  " not in state or nonState");}}}}	
	
	*/


public EquationValDrv evalVar(StochasticBasis model, double[] evalPt)throws ProjectionRuntimeException{
EquationValDrv another;
	
		  int varNum = model.getTheNonState().varPositionNSP(getVarName());

		  another=getEvaluationTimeAndType().doValSwitch( model,evalPt,varNum);

	    
	    return(another);}





public EquationValDrv evalVarNonStateGeneric(StochasticBasis model, Matrix nowMat, Matrix nxtMat, Matrix nowDrvMat, Matrix nxtDrvMat)throws ProjectionRuntimeException{
	  {
		
		int stateVarDim=model.getTheState().getStateVariablePolynomialWeights().getRowDimension();
		int nonStateVarDim;
		if(model.getTheNonState()==null) {nonStateVarDim=0;} else 
		    {nonStateVarDim=model.getTheNonState().getRelevantWeightsNSP().getRowDimension();};
			
		int nodeDim=model.getTheState().getStateVariablePolynomialWeights().getColumnDimension();
		int stateWtDim=nodeDim*stateVarDim;
		int nonStateWtDim=nodeDim*nonStateVarDim;

		int fullWtDim=stateWtDim+nonStateWtDim;
	    
	    int varNum=model.getTheNonState().varPositionNSP(getVarName());
		  EquationValDrv theRes=getEvaluationTimeAndType().doValSwitch( model,stateWtDim,fullWtDim,varNum,
					new Matrix(1,1),nowMat,nxtMat,
					nowDrvMat,nxtDrvMat);
					  return(theRes); }
	     }
	

public EquationValDrv evalDrvVar(Basis model, VarTime theDrvVar)
throws ProjectionRuntimeException{
 
	int theTime=getLagLead();
	int theDrvTime=theDrvVar.getLagLead();
	if(theDrvTime==theTime-1){
if(model.getTheNonState().hasVarQNSP(this)) {return(evalDrvVarNonState(model,theDrvVar));   }  else 
	{throw new ProjectionRuntimeException("CollocationSolution: var>>"+
			getVarName()+
	") not in state or nonState ");}}else 
	{
		throw new ProjectionRuntimeException("NonStateVarTime: only functional derivs of functions mapping t-1 state t or state at t t to t+1 implemented");
	}
	}	
	


public EquationValDrv evalDrvVarNonStateGeneric(Basis model, VarTime theDrvVar, double[][][] basisDrvs, double[][][] nxtDrvs)
throws ProjectionRuntimeException{
int theTime=getLagLead();  
	if(theTime==-1){
			throw new 
			ProjectionRuntimeException("CollocationSolution: error illegal lag of non state variable "+
					getVarName());} else {
						return(doDrvSwitchNonState( model, basisDrvs, nxtDrvs,theDrvVar));

						}

}

private EquationValDrv evalDrvVarNonState(Basis model, VarTime theDrvVar)
throws ProjectionRuntimeException{

	
	EquationValDrv another=evalDrvVarNonStateGeneric(model,theDrvVar,null,
		/*model.getTheState().getChebPolyDrvsWRTxTimeT()*/null);
		return(another);
		}


	

}
