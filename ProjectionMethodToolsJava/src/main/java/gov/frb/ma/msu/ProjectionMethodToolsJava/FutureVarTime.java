package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class FutureVarTime extends VarTimeType {

	@Override
	int getLagLead() {
		return(1);
	}

	EquationValDrv doValSwitch(StochasticBasis model,int nonStateOffset,int rightEnd,int varNum,
			Matrix laggedMat,Matrix nowMat,Matrix nxtMat,
			Matrix nowDrvMat,Matrix nxtDrvMat){
		  Matrix val;Matrix JJ;

		  int nonStateVarDim = model.getNonStateVarDim();
		  int numPolys = model.getNumPolys(this);

		  int nonStateWtDim=numPolys*nonStateVarDim;
		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
		

		  EquationValDrv newVals;
		  val = nxtMat.getMatrix(varNum*numPolys,(varNum+1)*numPolys-1,0,0);
		  JJ=
				 nxtDrvMat.getMatrix(nonStateOffset+varNum*numPolys,nonStateOffset+(varNum+1)*numPolys-1,0,rightEnd-1);
				 if(JJ.getColumnDimension()<fullWtDim)JJ=rightAugZeros(JJ,nonStateWtDim);
		newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}
	EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
			Matrix jacobianNxtState,String varName){
		Matrix val;Matrix JJ;
		int varNum = model.getTheState().getVarIndex(varName);
		int nodeDim=model.getNodeDim(this);
		int stateVarDim=model.getStateVarDim();
		int nonStateVarDim=model.getNonStateVarDim();

			val = nxtState.getMatrix(
					varNum*nodeDim,(varNum+1)*nodeDim-1,0,0);
			;
			JJ=rightAugZeros(
					jacobianNxtState.getMatrix(varNum*nodeDim,(varNum+1)*nodeDim-1,0,
							getStateWtDim(stateVarDim,nodeDim)-1),
					getNonStateWtDim(nonStateVarDim,nodeDim));
		return(new EquationValDrv(val,JJ));}
	
	EquationValDrv doDrvSwitchNonStateCorrect(Basis model,VarTime theDrvVar,String varName){
		Matrix val;Matrix JJ;
		int varNum = model.getTheState().getVarIndex(varName);
		int dVarNum=model.getDVarNum(theDrvVar);
		int numNodes=model.getNodeDim(this);

	
			val = model.getDrvPolyNowValMatrix(varNum,dVarNum,numNodes
					);
			JJ = model.getDrvPolyNowDrvMatrix(varNum,dVarNum,numNodes
					);
	
		return(new EquationValDrv(val,JJ));}

}
