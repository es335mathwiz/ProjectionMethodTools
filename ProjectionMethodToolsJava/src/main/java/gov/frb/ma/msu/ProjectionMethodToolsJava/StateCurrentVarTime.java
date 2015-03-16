package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class StateCurrentVarTime extends VarTimeType {

	
	

	@Override
	int getLagLead() {
		
		return 0;
	}


	EquationValDrv doValSwitch(StochasticBasis model,int varNum){
		  Matrix val;Matrix JJ;
int nonStateOffsetNot=0;int rightEndNot=model.getNumPolys(this)*model.getStateVarDim();
		  int numPolys = model.getNumPolys(this);
		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
		  EquationValDrv newVals;

		  Matrix nowMat=model.getTheState().getVariablesAtChebyshevNodesTimeT();
		  val = nowMat.getMatrix(0,numPolys-1,varNum,varNum);
		   
		  JJ = model.getTheState().getVariablesAtChebNodesTimeTDerivWRTWts();
		  
		  JJ=JJ.getMatrix(nonStateOffsetNot+varNum*numPolys,nonStateOffsetNot+(varNum+1)*numPolys-1,0,rightEndNot-1);
		  JJ = padZeroesOnRight(JJ,  fullWtDim);
		newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}


	EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
			Matrix jacobianNxtState,String varName){
		if(true)throw new ProjectionRuntimeException("not implemented for CurrentVarTime");
		return(new EquationValDrv());
	}

	

	

	EquationValDrv doValSwitch(StochasticBasis model,double[] evalPt,int varNum){
		StateVariablePolynomials svP=model.getTheState();
		  Matrix val;Matrix JJ;
double[] theRes=svP.evaluate(evalPt);
		  EquationValDrv newVals;
			  val=new Matrix(1,1,theRes[varNum]);
			  JJ=new Matrix(1,1);
			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);		  


	}


	EquationValDrv doDrvSwitchState(Basis model,double[] evalPt ,Matrix nxtState,
			Matrix jacobianNxtState,String varName){
		if(true)throw new ProjectionRuntimeException("not implemented for CurrentVarTime");
		return(new EquationValDrv());
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
