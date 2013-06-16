package gov.frb.ma.msu.ProjectionTools;

import java.util.Vector;

public class ChebyshevPolyWithEvalPoints extends ChebyshevPolynomial {

	protected Vector<double[]> evalPoints = new Vector<double []>();
	protected double [][] outerProdEvalPoints;
	protected double [][] xFormedOuterProdEvalPoints;
	private GridVars gridDef;
	protected int[] polyOrders;

	public ChebyshevPolyWithEvalPoints() {
		super();
	}

	public double[][] getOuterProdAtEvalPoints() {
		return outerProdEvalPoints;
	}

	public void setOuterProdAtEvalPoints(double[][] chebNodePoints) {
		this.outerProdEvalPoints = chebNodePoints;
	}

	public double[][] getXformedOuterProdEvalPoints() {
		return xFormedOuterProdEvalPoints;
	}

	public void setXFormedOuterProdEvalPoints(double[][] formedChebNodePoints) {
		xFormedOuterProdEvalPoints = formedChebNodePoints;
	}

	public Vector<double[]> getEvalPoints() {
		return evalPoints;
	}

	public void setEvalPoints(Vector<double[]> evalPoints) {
		this.evalPoints = evalPoints;
	}

	public GridVars getGridDef() {
		return gridDef;
	}

	public void setGridDef(GridVars gridDef) {
		this.gridDef = gridDef;
	}

	double[][] evaluateFirstDeriv(final int[] polyOrdersNow, final double [][] xx)
			throws ProjectionRuntimeException {
				final int numXVals=xx.length;
				final double [][] theRes=new double[numXVals][polyOrdersNow.length];
				int ii;
				for(ii=0;ii<numXVals;ii++) 
				{theRes[ii]=
					evaluateFirstDrvWRTx(polyOrdersNow,xx[ii],getTheMin(),getTheMax());}
				return(theRes);
			}

	public double [] getTheMin() {return(getGridDef().getMinVals());}

	public double [] getTheMax() {return(getGridDef().getMaxVals());}

	protected double evaluateBasisPolysAtPt(final int[] thePolyOrderss, final double [] xx)
			throws ProjectionRuntimeException {
				double theRes=1;
				chkLength(thePolyOrderss, xx);
				int ii;
				for(ii=0;ii<thePolyOrderss.length;ii++){
			
					theRes=theRes*ChebyshevPolysAtEvalPoints.recurFormula(thePolyOrderss[ii],
							toChebyshevInterval(xx[ii],getTheMin()[ii],getTheMax()[ii]));}
				return(theRes);
			}

}