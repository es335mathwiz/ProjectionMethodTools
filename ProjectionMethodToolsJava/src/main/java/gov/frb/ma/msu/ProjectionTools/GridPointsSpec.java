package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;
/**
 * characterizes the variables, ranges and orders of the polynomials
 */
public class GridPointsSpec {

	private GridVars theStateVars;
	private int[] theOrders;
	private ChebyshevPolysAtEvalPoints theChebPoly;
	private Matrix basisAtChebNodesAsMatrix;
/**
 * Uses user supplied vars and their orders.
 * sets chebyshevpoly at chebyshev points.
 * @param someVars GridVars
 * @param someOrders int[] should all be non-negative
 * @throws ProjectionRuntimeException
 */
	public GridPointsSpec(GridVars someVars, int[] someOrders)  throws ProjectionRuntimeException {
		theStateVars=someVars;
		theOrders=someOrders;
		setTheChebPoly(new ChebyshevPolysAtEvalPoints(someOrders,someVars));
		setBasisAtChebNodesAsMatrix();
	}
	public void print(){
		theStateVars.print();
		int ii;
		System.out.println("orders:");
		for(ii=0;ii<theOrders.length;ii++){
			System.out.println("ii="+ii+" " + theOrders[ii]);
		}
	}
/**
 * computes the total number of nodes for collocation for the powers associated with the grid
 * @return (k0+1)*(k2+1)*...(kn+1)
 */
	public int powersPlusOneProd(){
		int theRes=1;
		int ii; int len= theOrders.length;
		for(ii=0;ii<len;ii++){
			theRes=theRes*(1+theOrders[ii]);
		}
		return(theRes);
	}

	public int  numberOfGridVars(){
		return(getVariableNames().length);
	}

	public String [] getVariableNames(){
		return(theStateVars.getVariableNames());
	}
	/**
	 * provides a new grid with each of the variables restricted to a range
	 * in the middle of their old range
	 * @param frac GridPointsSpec
	 * @return GridPointsSpec
	 * @throws ProjectionRuntimeException
	 */
	public GridPointsSpec  theMiddle(double frac) throws ProjectionRuntimeException{
		return(new GridPointsSpec(theStateVars.theMiddle(frac),theOrders));
	}
	/**
	 * provides a new grid with each of the powers incremented by one unless
	 * they exceed the provided corresponding goal
	 * @param goalPowers
	 * @return GridPointsSpec
	 * @throws ProjectionRuntimeException
	 */
	public GridPointsSpec  incPowers(int [] goalPowers) throws ProjectionRuntimeException{
		GridPointsSpec incGrid=new GridPointsSpec(theStateVars,incOrders(theOrders,goalPowers));
		return(incGrid);}
	/**
	 * increases the current orders  by one for each variable that
	 * is less than the provided goal order
	 * @param oldOrders
	 * @param goalOrders
	 * @return int[]
	 */
	public static int [] incOrders(int[] oldOrders,int []goalOrders) {
		int ii;int numOrders=oldOrders.length;int [] newOrders = new int[numOrders];
		for(ii=0;ii<numOrders;ii++)
		{newOrders[ii]=Math.min(goalOrders[ii],oldOrders[ii]+1);}
		return(newOrders);
	}
	public int [] getOrders() {return(theOrders);}   
	public double [][] getRanges(){return(theStateVars.getRanges());}
	public Matrix getBasisAtChebNodesAsMatrix() {
		return basisAtChebNodesAsMatrix;
	}
	/**
	 * computes the number of points (and equations) in the 
	 * collocation polynomial equation system
	 * @return int
	 */
	int computeOuterProductUltimateSize() {
//		int numberOfStateVars=getTheChebPoly().getPolyOrders().length;
		int numberOfStateVars=getOrders().length;	
		int numberOfPolysInOuterProd=1;
		int ii;    
 
		for(ii=0;ii<numberOfStateVars;ii++){
			numberOfPolysInOuterProd=numberOfPolysInOuterProd*(getTheChebPoly().getPolyOrders()[ii]+1);
		}
		return(numberOfPolysInOuterProd);
	}
/**
 * generate a matrix with variable powers for each polynomial 
 * in the outer product in the "ProjectionMethodTools" order 
 * <p><blockquote><pre>
 * {@code
 * GridVarSpec [] vsArray = {new GridVarSpec("xx", 0, 2),
 * new GridVarSpec("yy", -5, 7),
 * new GridVarSpec("zz", 1, 70)};
 * GridVars theVars = new GridVars(vsArray);
 * int[] theOrds123 = {1, 2, 3};
 * GridPointSpec theGS123 = new GridPointsSpec(theVars,theOrds123);
 * 
 * 
 *produces
 * 0  0  0 
 * 1  0  0 
 * 0  1  0 
 * 1  1  0 
 * 0  2  0 
 * 1  2  0 
 * 0  0  1 
 * 1  0  1 
 * 0  1  1 
 * 1  1  1 
 * 0  2  1 
 * 1  2  1 
 * 0  0  2 
 * 1  0  2 
 * 0  1  2 
 * 1  1  2 
 * 0  2  2 
 * 1  2  2 
 * 0  0  3 
 * 1  0  3 
 * 0  1  3 
 * 1  1  3 
 * 0  2  3 
 * 1  2  3
 *}
 * </pre></blockquote></p>
 * @return int[][] of orders each row corresponds to a given polynomial
 * 
 */
	public int [][] generatePolyOrdersForOuterProduct() {
		int numberOfStateVars=getTheChebPoly().getPolyOrders().length;
		int numberOfPolysInOuterProd=1;
		int blkSizeNow;
		int reps;
		int ii;    

		numberOfPolysInOuterProd=computeOuterProductUltimateSize();
		int [][] aPolyOrders=new int[numberOfPolysInOuterProd][numberOfStateVars];

		int jj;int kk;int ll;int numberOfIthPolys;
		blkSizeNow=1;
		for(ii=0;ii<numberOfStateVars;ii++){
			numberOfIthPolys=getTheChebPoly().getPolyOrders()[ii]+1;
			reps=numberOfPolysInOuterProd/((numberOfIthPolys)*blkSizeNow);

			for(jj=0;jj<reps;jj++) {

				for(ll=0;ll<(numberOfIthPolys);ll++){
					for(kk=0;kk<blkSizeNow;kk++) {
						aPolyOrders[jj*blkSizeNow*(numberOfIthPolys)+ll*blkSizeNow+kk][ii]=ll;

					}
				}
			}blkSizeNow=blkSizeNow*(numberOfIthPolys);
		}
		return(aPolyOrders);
	}
/**
 * computes derivative of polynomials in "ProjectionMethodTools" order
 * wrt ???
 * not sure why this is here and not in statevariablepolynomials
 * @param polynomials
 * @return
 * @throws ProjectionRuntimeException
 */
	double [][][]
	            evaluateDrvsAtCurNodes(StateVariablePolynomials polynomials) throws ProjectionRuntimeException {

		int numberOfPolysInOuterProd=1;

		int ii;    

		numberOfPolysInOuterProd=computeOuterProductUltimateSize();
		int [][] aPolyOrders=generatePolyOrdersForOuterProduct();

		double [][][] theRes=new double[numberOfPolysInOuterProd][][];
		for(ii=0;ii<numberOfPolysInOuterProd;ii++){
			theRes[ii]=polynomials.evaluateDrvAtCurNodesHelp(aPolyOrders[ii]);	
		}
		return(theRes);
	}


	public int	getNumberOfShocks(){return(getTheStateVars().getNumberOfShocks());
	}

	public void setNumberOfShocks(int val){getTheStateVars().setNumberOfShocks(val);
	}
	public int getStateDimWithoutShocks(){
		return(getStateDim()-getNumberOfShocks());
	}
	public int getStateDim(){
		return(getTheStateVars().getTheVarSpecs().length);
	}

	private double [][] getXformedOuterProdEvalPoints(){
		return(getTheChebPoly().getXformedOuterProdEvalPoints());
	}
	public double [] getXformedChebNodePts(String varName)throws ProjectionRuntimeException{
		return(getTheChebPoly().getXFormedBasisAtEvalPoints(varName));
	}
	public int getVarIndex(String varName)throws ProjectionRuntimeException{
		return(getTheChebPoly().getVarIndex(varName));
	}

	public double [][] getBasisAtChebNodes(){
		return(getTheChebPoly().getBasisAtEvalPoints());
	}




	public void setBasisAtChebNodesAsMatrix(Matrix atNodesAsMatrix) {
		this.basisAtChebNodesAsMatrix = atNodesAsMatrix;
	}


	public void setBasisAtChebNodesAsMatrix() {

		double[][] asArray=getBasisAtChebNodes();
		basisAtChebNodesAsMatrix = new Matrix(asArray,asArray.length,asArray.length);

	}
	public ChebyshevPolysAtEvalPoints getTheChebPoly() {
		return theChebPoly;
	}
	public void setTheChebPoly(ChebyshevPolysAtEvalPoints theChebPoly) {
		this.theChebPoly = theChebPoly;
	}
	public int[] getTheOrders() {
		return theOrders;
	}
	public void setTheOrders(int[] theOrders) {
		this.theOrders = theOrders;
	}
	public GridVars getTheStateVars() {
		return theStateVars;
	}
	public void setTheStateVars(GridVars theVars) {
		this.theStateVars = theVars;
	}
	public int varPosition( String theVar) throws ProjectionRuntimeException {
		int ii;
		int thePos=-1;
		for(ii=0;ii<getVariableNames().length;ii++){
		    if(getVariableNames()[ii].equals(theVar)) {thePos=ii;break;}
		}
	    if(thePos<0) throw new 
	ProjectionRuntimeException("varPosition: var name not in CollocationPolynomial");
		return(thePos);
	    }
	public boolean hasVarQ(VarTime theVar) {
	String varName=theVar.getVarName();
	try {varPosition( varName);return(true);}
	catch(ProjectionRuntimeException ee) { return(false);}
	}

}
