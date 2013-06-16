package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;

public class NonStateCurrentVarTime extends VarTimeType {

	

	@Override
	int getLagLead() {
		
		return 0;
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
		  val = nowMat.getMatrix(varNum,varNum,0,numPolys-1).transpose();
		  JJ = nowDrvMat;
		  
		  JJ=JJ.getMatrix(nonStateOffset+varNum*numPolys,nonStateOffset+(varNum+1)*numPolys-1,0,rightEnd-1);
		  if(JJ.getColumnDimension()<fullWtDim)JJ=rightAugZeros(JJ,nonStateWtDim);
	

			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}

	EquationValDrv doValSwitch(StochasticBasis model,int varNum){
		  Matrix val;Matrix JJ;

		  int nonStateVarDim = model.getNonStateVarDim();
		  int numPolys = model.getNumPolys(this);

		  int nonStateWtDim=numPolys*nonStateVarDim;


		  int stateVarDim=model.getStateVarDim();

		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
			int nodeDim=model.getTheNonState().getRelevantWeightsNSP().getColumnDimension();
			int stateWtDim=nodeDim*stateVarDim;

 
Matrix nowMat=		model.getTheState().getVariablesAtChebyshevNodesTimeTNSP().transpose();
		  EquationValDrv newVals;
Matrix		  nowDrvMat=model.getTheState().getVariablesAtChebNodesTimeTDerivWRTWtsNSP();
		  val = nowMat.getMatrix(varNum,varNum,0,numPolys-1).transpose();
		  JJ = nowDrvMat;
		  
		  JJ=JJ.getMatrix(stateWtDim+varNum*numPolys,stateWtDim+(varNum+1)*numPolys-1,0,fullWtDim-1);
		  if(JJ.getColumnDimension()<fullWtDim)JJ=rightAugZeros(JJ,nonStateWtDim);
	

			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}
	

	
	
	
	
	
	

}
