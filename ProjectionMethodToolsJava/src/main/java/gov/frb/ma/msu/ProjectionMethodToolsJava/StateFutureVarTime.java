package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class StateFutureVarTime extends VarTimeType {

	@Override
	int getLagLead() {
		return(1);
	}

	
	

	EquationValDrv doValSwitch(StochasticBasis model,int varNum){
		  Matrix val;Matrix JJ;
		  int nonStateOffsetNot=0;int rightEndNot=model.getNumPolys(this)*model.getStateVarDim();
		  int nonStateVarDim = model.getNonStateVarDim();
		  int numPolys = model.getNumPolys(this);

		  int nonStateWtDim=numPolys*nonStateVarDim;
		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
		
Matrix nxtDrvMat=model.getTheState().getVariablesIteratedFromChebNodesTimeTP1DerivWRTWts();
		  EquationValDrv newVals;
		  Matrix nxtMat=model.getTheState().getVariablesIteratedFromChebNodesTimeTP1();
		  val = nxtMat.getMatrix(varNum*numPolys,(varNum+1)*numPolys-1,0,0);
		  JJ=
				 nxtDrvMat.getMatrix(nonStateOffsetNot+varNum*numPolys,nonStateOffsetNot+(varNum+1)*numPolys-1,0,rightEndNot-1);
				 if(JJ.getColumnDimension()<fullWtDim)JJ=rightAugZeros(JJ,nonStateWtDim);
		newVals=new EquationValDrv(val,JJ);

		  return(newVals);
	}
	
	
	
	
	EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
			Matrix jacobianNxtState,String varName){
		Matrix val;Matrix JJ;
		int varNum = model.getTheState().getTheGrid().varPosition(varName);


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
		

	
	
	
	
	
	
	

	EquationValDrv doValSwitch(StochasticBasis model,double[] evalPt,int varNum){
StateVariablePolynomials svP=model.getTheState();
		  Matrix val;Matrix JJ;
		  double[] shk ={0};
			double [] x$t =Utilities.augmentVecWithVal(svP.evaluate(evalPt),shk);
			double[] x$tp1=svP.evaluate(x$t);
 


		  EquationValDrv newVals;
			  val=new Matrix(1,1,x$tp1[varNum]);
			  JJ=new Matrix(1,1);
			  newVals=new EquationValDrv(val,JJ);

		  return(newVals);		  

	}
	
	
	
	
	EquationValDrv doDrvSwitchState(Basis model,double[] evalPt,Matrix nxtState,
			Matrix jacobianNxtState,String varName){

		if(true)throw new ProjectionRuntimeException("not implemented for CurrentVarTime");
		return(new EquationValDrv());
		}
		

	
	
	
	
	
	
	
	
	
	
	
	
}
