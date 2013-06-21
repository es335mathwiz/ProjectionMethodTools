package gov.frb.ma.msu.ProjectionTools;

public abstract class DoEqns {
	public abstract
	void updateParams(double [] paramVec);
public abstract	double [] getParams();
    public abstract 
	EquationValDrv updateValDrv(StochasticBasis theProjModel) 
    throws ProjectionRuntimeException;
		
	public  void noNans(
				  WeightedStochasticBasis solution, double[][] xx, double[][] dblDelta) throws ProjectionRuntimeException{
		int stateVarDim=solution.getStateVarDim();
		int nonStateVarDim;
		if(solution.getTheNonState()==null) {nonStateVarDim=0;} else 
		    {nonStateVarDim=solution.getNonStateVarDim();};
		
		int nodeDim=0;
		if(solution.getTheState().getStateVariablePolynomialWeights()!=null)
		nodeDim=solution.getTheState().getStateVariablePolynomialWeights().getColumnDimension(); else
		nodeDim=solution.getTheNonState().getRelevantWeightsNSP().getColumnDimension();
		double [][] stateXX = new double[stateVarDim][nodeDim];
		double [][] nonStateXX = new double[nonStateVarDim][nodeDim];

		moveXXAlongNewtonDirection(xx, dblDelta);
		Utilities.copyXxToState(xx, stateVarDim, nodeDim, stateXX);
		Utilities.copyXxToNonState(xx, stateVarDim, nonStateVarDim, nodeDim, nonStateXX);
		    try {
		solution.updatePolyWeights(this, stateXX, nonStateXX);
		    } catch (ProjectionRuntimeException ee) {
		    	throw new ProjectionRuntimeException("from doEqns:"+ee.getMessage());
		    }
		    return;
	}

	void moveXXAlongNewtonDirection(double[][] xx, double[][] dblDelta) {
			int ii,jj;
			for(ii=0;ii<xx.length;ii++){
			for(jj=0;jj<xx[0].length;jj++){
			    xx[ii][jj]=xx[ii][jj] - dblDelta[ii*xx[0].length+jj][0];
			
			}}
		}
}
