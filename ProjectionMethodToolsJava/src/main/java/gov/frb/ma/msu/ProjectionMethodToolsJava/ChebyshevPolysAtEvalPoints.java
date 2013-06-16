package gov.frb.ma.msu.ProjectionMethodToolsJava;
/**
 * Evaluates Chebyshev polynomials and derivatives at Chebyshev nodes (both untransformed and transformed)
 * Organizes their outer product in ProjectionMethodToolsOrder
 * @see GridVarSpec
 * @author m1gsa00
 *
 */
public class ChebyshevPolysAtEvalPoints extends ChebyshevPolyWithEvalPoints {
	private double [][] orderedBasisAtOuterProdEvalPoints;
	private double [][][] orderedBasisDrvsAtOuterProdEvalPoints;
	private int [][] orderedPolyPowTuples; 	
	
	public ChebyshevPolysAtEvalPoints(final int[] order,final GridVars theVarSpecs) throws ProjectionRuntimeException{
		setGridDef(theVarSpecs);
		setPolyOrders(order);
		int ii;
		for(ii=0;ii<order.length;ii++) {
				evalPoints.add(chebyshevNodes(order[ii]+1));
		}
		setOrderedPolyPowTuples(generateOrderedTableOfTuples(getNumberOfPolys(),getPolyOrders()));
		setOuterProdEvalPoints(computeOuterProdEvalPoints(getNumberOfPolys(),getNumGridVars(),getEvalPoints()));
		setXFormedOuterProdEvalPoints(computeLinXform(getNumberOfPolys(),getNumGridVars(),
				getTheMin(),getTheMax(),getOuterProdEvalPoints()));
		setOrderedBasisAtOuterProdEvalPoints(evaluateBasisAtEvalPoints(getPolyOrders(),
				getOuterProdEvalPoints(),getOrderedPolyPowTuples()));
		setOrderedBasisDrvsAtOuterProdEvalPoints(
		evaluateBasisDrvsAtEvalPoints(getNumberOfPolys(),getOrderedPolyPowTuples(),getOuterProdEvalPoints()));

	}





/**
 * Evaluates Chebyshev polynomials at a given point
 * @param xx
 * @return
 * @throws ProjectionRuntimeException
 */
public  double []
	                evaluateBasisPolysAtPt(final double [] xx) throws ProjectionRuntimeException {
		int ii;    

		final double [] theRes=new double[getNumberOfPolys()];
		for(ii=0;ii<getNumberOfPolys();ii++){
			theRes[ii]=evaluateBasisPolysAtPt(getOrderedPolyPowTuples()[ii],xx);	
		}
		return(theRes);
	}

/**
 * Evaluates first derivative with respect to polynomial variables at the at a given point
 * @param xx
 * @return
 * @throws ProjectionRuntimeException
 */
	public  double [][]
	                  evaluateBasisPolysDrvsAtPt(final double [] xx) throws ProjectionRuntimeException {

		int ii;    

		final double [][] theRes=new double[getNumberOfPolys()][];
		for(ii=0;ii<getNumberOfPolys();ii++){
			theRes[ii]=evaluateFirstDrvWRTx(getOrderedPolyPowTuples()[ii],xx,getTheMin(),getTheMax());	
		}
		return(theRes);
	}
/**
 * Computes and returns the transformed Chebyshev node values for a given variable at the Chebyshev node points.
 * @param varName
 * @return
 * @throws ProjectionRuntimeException
 */
	public double [] getXFormedBasisAtEvalPoints(final String varName)throws ProjectionRuntimeException{
		final int numPts=xFormedOuterProdEvalPoints.length;
		final double [] aColumn=new double[numPts];
		final int theCol=getVarIndex(varName);
		int ii;
		for(ii=0;ii<numPts;ii++){
			aColumn[ii]=xFormedOuterProdEvalPoints[ii][theCol];
		}
		return(aColumn);
	}



	public double [][] getBasisAtEvalPoints(){
		return(orderedBasisAtOuterProdEvalPoints);
	}




	public void setBasisAtEvalPoints(double[][] basisAtChebNodes) {
		this.orderedBasisAtOuterProdEvalPoints = basisAtChebNodes;
	}

	public void setBasisDrvsAtEvalPoints(double[][][] basisDrvsAtChebNodes) {
		this.orderedBasisDrvsAtOuterProdEvalPoints = basisDrvsAtChebNodes;
	}

	public double[][][] getBasisDrvsAtEvalPoints() {
		return orderedBasisDrvsAtOuterProdEvalPoints;
	}
	
	public int[][] getOrderedPolyPowTuples() {
		return orderedPolyPowTuples;
	}

	public void setOrderedPolyPowTuples(int[][] orderedTuples) {
		this.orderedPolyPowTuples = orderedTuples;
	}
	public double[][] getOuterProdEvalPoints() {
		return outerProdEvalPoints;
	}
	public void setOuterProdEvalPoints(double[][] outerProdEvalPoints) {
		this.outerProdEvalPoints = outerProdEvalPoints;
	}
	public void setxFormedOuterProdEvalPoints(double[][] xFormedOuterProdEvalPoints) {
		this.xFormedOuterProdEvalPoints = xFormedOuterProdEvalPoints;
	}
	public double[][] getOrderedBasisAtOuterProdEvalPoints() {
		return orderedBasisAtOuterProdEvalPoints;
	}
	public void setOrderedBasisAtOuterProdEvalPoints(
			double[][] orderedBasisAtOuterProdEvalPoints) {
		this.orderedBasisAtOuterProdEvalPoints = orderedBasisAtOuterProdEvalPoints;
	}
	public double[][][] getOrderedBasisDrvsAtOuterProdEvalPoints() {
		return orderedBasisDrvsAtOuterProdEvalPoints;
	}
	public void setOrderedBasisDrvsAtOuterProdEvalPoints(
			double[][][] orderedBasisDrvsAtOuterProdEvalPoints) {
		this.orderedBasisDrvsAtOuterProdEvalPoints = orderedBasisDrvsAtOuterProdEvalPoints;
	}
	
	protected int getNumberOfPolys() {
		final int icnt=getPolyOrders().length;
		int ultSize=1;
		int ii;    
		for(ii=0;ii<icnt;ii++){
			ultSize=ultSize*(getPolyOrders()[ii]+1);
		}
		return(ultSize);
	}

	public int[] getPolyOrders() {
		return polyOrders;
	}
	public void setPolyOrders(int[] polyOrders) {
		this.polyOrders = polyOrders;
	}
	public int getVarIndex(final String varName) throws ProjectionRuntimeException {
	
		int it;
		int theRes=-1;int icnt=0;
	
		for(it=0;it<getPolyVarNames().length;it++)  {
	
			if(varName.equals(getPolyVarNames()[it])) {
				theRes=icnt;
			}
			icnt++;
		}
		if(theRes<0) {throw new
			ProjectionRuntimeException("chebshevPoly: varName=" + varName +" not in chebyshev poly");} else {
				return(theRes);
			}
	}
	public String getVarName(final int indx) {
	
		return(getPolyVarNames()[indx]);
	}
	public String[] getPolyVarNames() {
		return (getGridDef().getVariableNames());
	}

	public String[] getTheVarNames(){return(getGridDef().getVariableNames());}
	public int 	getNumGridVars(){return(getGridDef().getVariableNames().length);}	



}
