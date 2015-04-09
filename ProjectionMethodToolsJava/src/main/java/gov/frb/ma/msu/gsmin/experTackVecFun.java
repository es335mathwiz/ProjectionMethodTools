package gov.frb.ma.msu.gsmin;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import gov.frb.ma.msu.gsmin.FAndG;
import gov.frb.ma.msu.gsmin.ValueDerivative;
import Jama.Matrix;
public class experTackVecFun extends FAndG{

	 DoEqns tbm;
	 notExperTackCnstrns tcn;
	 WeightedStochasticBasis thePM;

	 	public ValueDerivative evaluateF(Matrix xx){return(evaluateSumSq(xx));}
	 	public ValueDerivative evaluateG(Matrix xx){return(evaluateCnstrns(xx));};


	
		 public experTackVecFun(DoEqns theSys,notExperTackCnstrns theCnstrns,WeightedStochasticBasis theProjModel){
	tbm= theSys;
	tcn=theCnstrns;
	thePM=theProjModel;
		 }
		 public ValueDerivative	 evaluate(Matrix xxMat){
			 EquationValDrv theEqValDrv;
			updateXXs(xxMat);
			 
		theEqValDrv=tbm.updateValDrv(thePM);
			 return(new ValueDerivative(
										theEqValDrv.theVal.transpose().getArray(),theEqValDrv.theJac.transpose().getArray()));
		 };
		 public ValueDerivative	 evaluateSumSq(Matrix xxMat){
			 EquationValDrv theEqValDrv;
			 updateXXs(xxMat);
			 theEqValDrv=	tbm.updateValDrv(thePM);
				return(theEqValDrv.sumSq());
		 }
		 public ValueDerivative	 evaluateCnstrns(Matrix xxMat){
			 EquationValDrv theEqValDrv;
			 theEqValDrv=		tcn.updateValDrv(thePM);
				return(theEqValDrv.stackCnstrns());
		 }
		public void updateXXs(Matrix xxMat){
			 
//			 eqValDrv theEqValDrv;
				int stateVarDim=thePM.getTheState().getStateVariableNames().length;
				int nonStateVarDim;int shocksDim;
				if(thePM.getTheNonState()==null) {nonStateVarDim=0;} else 
				    {nonStateVarDim=thePM.getTheNonState().getNonStateVarDim();};
//				    if(thePM.getTheShocks()==null) {shocksDim=0;} else 
//				    {shocksDim=thePM.getTheShocks().getVarNames().length;};
				    double[][] xx=unVec(xxMat,stateVarDim+nonStateVarDim).getArrayCopy();				int nodeDim=xx[0].length;
				int ii;int jj;int icnt=0;
	
				double [][] stateXX = new double[stateVarDim][nodeDim];
				double [][] nonStateXX = new double[nonStateVarDim][nodeDim];

				for(ii=0;ii<stateVarDim;ii++){
				for(jj=0;jj<nodeDim;jj++){
				    stateXX[ii][jj]=xx[ii][jj];
				}}
				for(ii=0;ii<nonStateVarDim;ii++){
				for(jj=0;jj<nodeDim;jj++){
				    nonStateXX[ii][jj]=xx[ii+stateVarDim][jj];
				    icnt++;
				}}

			thePM.setTheStateWeights(stateXX);
				if(thePM.getTheNonState() != null){
					thePM.setTheNonStateWeights(nonStateXX);}

		
		 }
}
		 


