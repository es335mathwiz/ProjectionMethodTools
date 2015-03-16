package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class StateLaggedVarTime extends VarTimeType {
	
	public int getLagLead(){
		return(-1);
	}


public	EquationValDrv doValSwitch(StochasticBasis model,int varNum){

		  Matrix val;Matrix JJ;

		  int numPolys = model.getNumPolys(this);

		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
		
		  int numEvalPts=model.getNumEvalPts(this);
		  EquationValDrv newVals;
Matrix laggedMat=new Matrix(model.getTheState().getXformedChebNodePts());
			  val=laggedMat.getMatrix(0,numPolys-1,varNum,varNum);
			  JJ=new Matrix(numEvalPts,fullWtDim);
			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}


public	EquationValDrv doValSwitch(StochasticBasis model,double[] evalPt,int varNum){

		  Matrix val;Matrix JJ;

		  EquationValDrv newVals;
			  val=new Matrix(1,1,evalPt[varNum]);
			  JJ=new Matrix(1,1);
			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}


	EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
			Matrix jacobianNxtState,String varName){

			throw new ProjectionRuntimeException("CollocationSolution: functional derivs not implementd for lagged values");
	}



}
