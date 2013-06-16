package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class LaggedVarTime extends VarTimeType{
public int getLagLead(){
	return(-1);
}

EquationValDrv doValSwitch(StochasticBasis model,int nonStateOffset,int rightEnd,int varNum,
		Matrix laggedMat,Matrix nowMat,Matrix nxtMat,
		Matrix nowDrvMat,Matrix nxtDrvMat){
	  Matrix val;Matrix JJ;

	  int numPolys = model.getNumPolys(this);

	  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
	
	  int numEvalPts=model.getNumEvalPts(this);
	  EquationValDrv newVals;

		  val=laggedMat.getMatrix(0,numPolys-1,varNum,varNum);
		  JJ=new Matrix(numEvalPts,fullWtDim);
		  newVals=new EquationValDrv(val,JJ);

	  return(newVals);
}

}
