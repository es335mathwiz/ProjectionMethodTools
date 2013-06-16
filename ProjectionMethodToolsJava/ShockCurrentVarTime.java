package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;

public class ShockCurrentVarTime extends StateCurrentVarTime {

	EquationValDrv doValSwitch(StochasticBasis model,int varNum){
	
			  Matrix val;
			  Matrix JJ;
			  int numPolys = model.getNumPolys(this);
			  int fullWtDim=model.getNumStatePlusNonStatePolys(this);
			  EquationValDrv newVals;
			  double [] svs=model.getTheShockVals();
		      int varLoc=model.getShockVarLocs()[varNum];
			  val=new Matrix(numPolys,1,svs[varLoc]);
			  JJ=new Matrix(numPolys,fullWtDim);		
//				   JJ.set(0, varNum, 1);			   
			  newVals=new EquationValDrv(val,JJ);
			  return(newVals);
		}
		
			
		EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
				Matrix jacobianNxtState,String varName){
			if(true)throw new ProjectionRuntimeException("not implemented for CurrentVarTime");
			return(new EquationValDrv());	}

		

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

	

}
