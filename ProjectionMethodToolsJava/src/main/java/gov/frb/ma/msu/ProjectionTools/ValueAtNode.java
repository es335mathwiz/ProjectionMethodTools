package gov.frb.ma.msu.ProjectionTools;



public class ValueAtNode extends VarTime {


	public ValueAtNode(String aName){
		setVarName(aName);
/*		setVarTime(0);*/
	}
/*	public void setVarTime(int aTime){
		setEvaluationTimeAndType(
				varTimeType.newVarTimeType(aTime,
						varTimeType.getSTATE(),varTimeType.getNOTDRV()));
	}
*/
	public EquationValDrv evalVar(StochasticBasis model)throws ProjectionRuntimeException{
		if(model.getTheState().getTheGrid().hasVarQ(this)) {	
			return(evalVarState(model));
		}  
		else {
			{throw new ProjectionRuntimeException(
					"CollocationSolution: var>>"+
					getVarName()+
			") not in state");}}}


	public EquationValDrv evalVarState(StochasticBasis model)throws ProjectionRuntimeException{
		int varNum = getVarNum(model,this);
		EquationValDrv theRes=getEvaluationTimeAndType().doValSwitch( model,varNum);
		return(theRes); }

	public EquationValDrv evalDrvVar(Basis model, VarTime theDrvVar)
	throws ProjectionRuntimeException{

		int theTime=getLagLead();
		int theDrvTime=theDrvVar.getLagLead();
		if(theDrvTime==theTime-1){
			if(model.getTheState().getTheGrid().hasVarQ(this)) {
				return(evalDrvVarState(model,theDrvVar));   }  else 
				{throw new ProjectionRuntimeException(
						"CollocationSolution: var>>"+
						getVarName()+
				") not in state or nonState ");}}else 
				{
					throw new ProjectionRuntimeException("ValueAtNode: only functional derivs of functions mapping t-1 state t or state at t t to t+1 implemented");
				}
	}	

	public EquationValDrv evalDrvVarState(Basis model, VarTime theDrvVar)
	throws ProjectionRuntimeException{
		String theName=getVarName();
		String theDrvName=theDrvVar.getVarName();
//		double[][] theDrvBasis = getTheDrvBasis(model, theName, theDrvName);

		EquationValDrv another=evalDrvVarStateGeneric(model,theDrvVar,
				model.getTheState().computeNxtStateAtNodes(),model.getTheState().computeJacobianNxtStateAtNodes());
		return(another);

	}

	}


