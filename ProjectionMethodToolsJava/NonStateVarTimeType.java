package gov.frb.ma.msu.ProjectionTools;
import Jama.Matrix;
public abstract class NonStateVarTimeType extends VarTimeType {

	public NonStateVarTimeType() {
		super();
	}

	public EquationValDrv doValSwitchNot(StochasticBasis model, int varNum,Matrix nowMat,Matrix nowDrvMat) {
		  Matrix val;Matrix JJ;
	
		  int nonStateVarDim = model.getNonStateVarDim();
		  int numPolys = model.getNumPolys(this);
	
		  int nonStateWtDim=numPolys*nonStateVarDim;
		  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
		
	
		  EquationValDrv newVals;
	
		  val = nowMat.getMatrix(varNum,varNum,0,numPolys-1).transpose();
		  JJ = nowDrvMat;
		  nonStateVarDim=model.getTheState().getStateVariablePolynomialWeights().getRowDimension();
			
			int nodeDim=model.getTheState().getStateVariablePolynomialWeights().getColumnDimension();
			int stateVarDim=model.getTheState().getStateVariablePolynomialWeights().getRowDimension();
		  int nonStateOffset=nodeDim*stateVarDim;
	
	
		  JJ=JJ.getMatrix(nonStateOffset+varNum*numPolys,nonStateOffset+(varNum+1)*numPolys-1,0,fullWtDim-1);
		  if(JJ.getColumnDimension()<fullWtDim)JJ=rightAugZeros(JJ,nonStateWtDim);
	
	
			  newVals=new EquationValDrv(val,JJ);
	
		  return(newVals);
	}

}
