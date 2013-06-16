package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;


public abstract class VarTime {
    private String varName;
    private VarTimeType evaluationTimeAndType;
    public VarTime(){
	super();
	varName="defaultName";
	evaluationTimeAndType=new CurrentVarTime();
    }
    
    public String getVarName(){
	return(varName);
    }
    public int getLagLead(){
	return(evaluationTimeAndType.getLagLead());
    }

public EquationValDrv doValSwitch( StochasticBasis model, int nonStateOffset, int rightEnd, int varNum, Matrix laggedMat, Matrix nowMat, Matrix nxtMat, Matrix nowDrvMat, Matrix nxtDrvMat){
return evaluationTimeAndType.doValSwitch( model, nonStateOffset, rightEnd, varNum, laggedMat, nowMat, nxtMat, nowDrvMat, nxtDrvMat);}
	


	int getNumPolys(Basis model) {
		int numPolys=model.getTheState().getTheGrid().powersPlusOneProd();
		return numPolys;
	}
	int getNonStateVarDim(Basis model) {
		int nonStateVarDim;
		if(model.getTheNonState()==null) {nonStateVarDim=0;} else 
		    {nonStateVarDim=model.getTheNonState().getRelevantWeightsNSP().getRowDimension();};
		return nonStateVarDim;
	}
	int getStateVarDim(Basis model) {
		int stateVarDim=model.getTheState().getStateVariablePolynomialWeights().getRowDimension();
		return stateVarDim;
	}
	int getVarNum(Basis model,VarTime theVar) {
		int varNum=model.getTheState().getTheGrid().varPosition( theVar.getVarName());
		return varNum;
	}
	  int getNumEvalPts(Basis model) {
		int numEvalPts;
		numEvalPts=getNumPolys(model);return(numEvalPts);
	}
	Matrix rightAugZeros(Matrix theMat,int zeroCols) {
	if(zeroCols==0){return(theMat);} else {
	double [][] dblMat=theMat.getArray();
	int rowDim=dblMat.length;
	int colDim=dblMat[0].length;
	double [][] resMat= new double [rowDim][colDim+zeroCols];
	int ii;int jj;
	for(ii=0;ii<rowDim;ii++){
	for(jj=0;jj<colDim;jj++){
	    resMat[ii][jj]=dblMat[ii][jj];
	}}
	return(new Matrix(resMat));}}
 
	
//	public double[][] getTheDrvBasis(Basis model, String theName, String theDrvName) {
//		double [][] theDrvBasis=model.getBasisDrvsAtChebNodes(theName,theDrvName);
		//return theDrvBasis;
//		return null;
//	}

	
	public EquationValDrv evalDrvVarStateGeneric(Basis model, VarTime theDrvVar, Matrix nxtState, Matrix jacobianNxtState)
	throws ProjectionRuntimeException{

	

			return(doDrvSwitchState(model,nxtState,jacobianNxtState));
		
			
}
	
	
	EquationValDrv doDrvSwitchState(Basis model,
			Matrix nxtState,Matrix jacobianNxtState){
		return(evaluationTimeAndType.doDrvSwitchState( model,nxtState,
				 jacobianNxtState,getVarName()));
	}
	


	public void setVarName(String varName) {
		this.varName = varName;
	}
	public void setEvaluationTimeAndType(VarTimeType timeOffset) {
		this.evaluationTimeAndType = timeOffset;
	}


	EquationValDrv doDrvSwitchNonState(Basis model,double[][][] basisDrvs, double [][][] nxtDrvs,VarTime theDrvVar){
		return(evaluationTimeAndType.doDrvSwitchNonStateCorrect( model,theDrvVar,getVarName()));
	}

	public VarTimeType getEvaluationTimeAndType() {
		return evaluationTimeAndType;
	}
						 
	  
}
