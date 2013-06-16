package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;

public class CurrentVarTime extends VarTimeType {

	@Override
	int getLagLead() {
		
		return 0;
	}

	EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
			Matrix jacobianNxtState,String varName){
if(true)throw new ProjectionRuntimeException("not implemented for CurrentVarTime");
		return(new EquationValDrv());}

	EquationValDrv doDrvSwitchNonState(Basis model,double[][][] basisDrvs,
			double [][][] nxtDrvs,VarTime theDrvVar,String varName){
		Matrix val;Matrix JJ;
		int varNum = model.getTheState().getTheGrid().getVarIndex(varName);
		int dVarNum=model.getDVarNum(theDrvVar);
		int numNodes=model.getNodeDim(this);

			val = model.getDrvPolyNowValMatrix(varNum,dVarNum,numNodes);		
			JJ = model.getDrvPolyNowDrvMatrix(varNum,dVarNum,numNodes);
		return(new EquationValDrv(val,JJ));}
	
}
