package gov.frb.ma.msu.ProjectionTools;

import java.util.Vector;

public class ChebyshevPolynomial {

	public static double toChebyshevInterval(final double aVal, final double theMin, final double theMax) {
		return((2*(aVal-theMin)/(theMax-theMin))-1);
	}

	public static double fromChebyshevInterval(final double zVal, final double theMin, final double theMax) {
		return(((zVal + 1)*(theMax-theMin)/2)+theMin);
	}

	protected static double [] chebyshevNodes(final int nn) {
		final double [] theNodes = new double[nn];
		int kk;
		for(kk=0;kk<nn;kk++) {
			theNodes[kk]=Math.cos(((2*(kk+1)- 1)*Math.PI)/(2*nn)); 
		}
		return(theNodes);
	}

	protected static double recurFormula(final int nn, final double xx) {
		double theRes;
		switch (nn) {
		case 0: theRes=1;break;
		case 1: theRes=xx;break;
		default: theRes= 2 * xx * recurFormula(nn-1,xx)-recurFormula(nn-2,xx);
		break;
		}
		return(theRes);
	}

	protected static double recurFormulaSecond(final int nn, final double xx) {
		double theRes=0;
		switch (nn) {
		case -1: theRes=0;break;
		default: theRes= xx*recurFormulaSecond(nn-1,xx)+recurFormula(nn,xx);
		break;
		}
		return(theRes);
	
	}

	public ChebyshevPolynomial() {
		super();
	}

	protected static double [] fromChebyshevInterval(final double[] zVal, final double[] theMin, final double[] theMax) {
		final double[] theRes= new double[zVal.length];
		int ii;
		for(ii=0;ii<zVal.length;ii++){
			theRes[ii]=fromChebyshevInterval(zVal[ii],theMin[ii],theMax[ii]);
		}
		return(theRes);
	
	}

	public static int[][] generateOrderedTableOfTuples(int numPols, int[] orders) {
		int blkSizeNow;
		int reps;
		int ii;    
		final int [][] aPolyOrder=new int[numPols][orders.length];
			int jj;int kk;int ll;
		blkSizeNow=1;
		for(ii=0;ii<orders.length;ii++){
			reps=numPols/((orders[ii]+1)*blkSizeNow);
			for(jj=0;jj<reps;jj++) {
				for(ll=0;ll<(orders[ii]+1);ll++){
					for(kk=0;kk<blkSizeNow;kk++) {
						aPolyOrder[jj*blkSizeNow*(orders[ii]+1)+ll*blkSizeNow+kk][ii]=ll;
					}
				}
			}blkSizeNow=blkSizeNow*(orders[ii]+1);
		}
		return(aPolyOrder);
	}

	public static double[][] computeOuterProdEvalPoints(int numPols, int numVars,
			Vector<double[]> thePts) throws ProjectionRuntimeException {
					final double [][] theRes=new double[numVars][];
					double [][] outerProdEvalPointsXX=new double[numPols][numVars];
					int it;
					int reps;
					final int[] theSizes=new int[numVars];
					double[] ptsNow;
					for(it=0;it<numVars;it++){	
						ptsNow=thePts.get(it);
						theRes[it]=ptsNow;theSizes[it]=ptsNow.length;
					}
					int ii;    
					int jj;int kk;int ll;
					int blkSizeNow=1;
					for(ii=0;ii<numVars;ii++){
							reps=numPols/(theSizes[ii]*blkSizeNow);
							for(jj=0;jj<reps;jj++) {
								for(ll=0;ll<theSizes[ii];ll++){
								for(kk=0;kk<blkSizeNow;kk++) {
								outerProdEvalPointsXX[jj*blkSizeNow*theSizes[ii]+ll*blkSizeNow+kk][ii]=theRes[ii][ll];						}
							}
						}blkSizeNow=blkSizeNow*theSizes[ii];
						}
			return(outerProdEvalPointsXX);
					}

	protected static double[][] computeLinXform(int numPols, int numVars,
			double[] aMin, double [] aMax, double [][]orig) {
				int ii;double[][]xFormed=new double[numPols][numVars];
				for(ii=0;ii<numPols;ii++){
				xFormed[ii]=ChebyshevPolynomial.fromChebyshevInterval(
						orig[ii],aMin,aMax);
				}
				return(xFormed);
			}

	public static double[][] evaluateBasisAtEvalPoints(final int[] thePolyOrders, double[][] evalPts,
			int[][]orderedTups) throws ProjectionRuntimeException {
				int ii;  
				final double [][] theRes=new double[evalPts.length][];
				for(ii=0;ii<evalPts.length;ii++){
					theRes[ii]=evaluateBasisAtEvalPoints(orderedTups[ii],evalPts);	
				}
				return(theRes);
			}

	public static double [] evaluateBasisAtEvalPoints(final int[] thePolyOrders, double[][] evalPts)
			throws ProjectionRuntimeException {
				final double [] theRes=new double[evalPts.length];
				int ioutpt;
				double [] xx;
				for(ioutpt=0;ioutpt<evalPts.length;ioutpt++){
					theRes[ioutpt]=1;
					xx=evalPts[ioutpt];
					chkLength(thePolyOrders, xx);
					int ii;
					for(ii=0;ii<thePolyOrders.length;ii++){
						theRes[ioutpt]=theRes[ioutpt]*
						ChebyshevPolysAtEvalPoints.recurFormula(thePolyOrders[ii],xx[ii]);
					}
				}
				return(theRes);
			}

	protected static void chkLength(final int[] thePolyOrders, double[] xx) {
		if(xx.length != thePolyOrders.length) {
			throw new 
			ProjectionRuntimeException("xx length="+ xx.length+" different length than thePolyOrders length=" +thePolyOrders.length);
		}
	}

	public static double[][][] evaluateBasisDrvsAtEvalPoints(int numPols,
			int[][] ordTups, double[][] outerEvalPts) throws ProjectionRuntimeException {
				int ii;    
				final double [][][] theRes=new double[numPols][][];
				for(ii=0;ii<numPols;ii++){
					theRes[ii]=evaluateBasisDrvAtEvalPoints(numPols,ordTups[ii],outerEvalPts);	
				}
				return(theRes);
			}

	public static double [][] evaluateBasisDrvAtEvalPoints(int numPols,
			final int[] thePolyOrders, double[][]outerEvalPts) throws ProjectionRuntimeException {
				final double [][] theRes=new double[outerEvalPts.length][thePolyOrders.length];
				int ioutpt;
				chkLength(thePolyOrders, outerEvalPts[0]);
				for(ioutpt=0;ioutpt<outerEvalPts.length;ioutpt++){
					theRes[ioutpt]=
						evaluateBasisFirstDerivBasisAtEvalPoints(thePolyOrders,outerEvalPts[ioutpt]);
				}
				return(theRes);
			}

	public static double [] evaluateBasisFirstDerivBasisAtEvalPoints(final int[] polyOrderNow,
			final double [] xx) throws ProjectionRuntimeException {
				final double [] theRes=new double[polyOrderNow.length];
				final double [] forProdLevel=new double[polyOrderNow.length];
				final double [] forProdDeriv=new double[polyOrderNow.length];
				chkLength(polyOrderNow, xx);
				int ii;
				for(ii=0;ii<polyOrderNow.length;ii++){
					forProdLevel[ii]=ChebyshevPolysAtEvalPoints.recurFormula(polyOrderNow[ii],xx[ii]
					);
					forProdDeriv[ii]=
						polyOrderNow[ii]*ChebyshevPolysAtEvalPoints.recurFormulaSecond(polyOrderNow[ii]-1,
								xx[ii]);
					theRes[ii]=1;
				}
				int jj;
				for(ii=0;ii<polyOrderNow.length;ii++){
					for(jj=0;jj<polyOrderNow.length;jj++){
						if(ii==jj) {
							theRes[ii]=theRes[ii]*forProdDeriv[jj];}
						else {
							theRes[ii]=theRes[ii]*forProdLevel[jj];}
					}
				}
				return(theRes);
			}

	static	public double [] evaluateFirstDrvWRTx(final int[] polyOrdersNow, final double [] xx,
	double[] mins,double[]maxs)
	throws ProjectionRuntimeException {
		final double [] theRes=new double[polyOrdersNow.length];
		final double [] forProdLevel=new double[polyOrdersNow.length];
		final double [] forProdDeriv=new double[polyOrdersNow.length];
		if(xx.length != polyOrdersNow.length) {
			throw new 
			ProjectionRuntimeException("xx different length than polyOrdersNow");
		}
		int ii;
		for(ii=0;ii<polyOrdersNow.length;ii++){
	
			forProdLevel[ii]=recurFormula(polyOrdersNow[ii],
					toChebyshevInterval(xx[ii],mins[ii],maxs[ii]));
			forProdDeriv[ii]=
				polyOrdersNow[ii]*recurFormulaSecond(polyOrdersNow[ii]-1,
						toChebyshevInterval(xx[ii],mins[ii],maxs[ii]));
			theRes[ii]=2/(maxs[ii]-mins[ii]);
		}
		int jj;
		for(ii=0;ii<polyOrdersNow.length;ii++){
	
			for(jj=0;jj<polyOrdersNow.length;jj++){
				if(ii==jj) {
					theRes[ii]=theRes[ii]*forProdDeriv[jj];}
				else {
					theRes[ii]=theRes[ii]*forProdLevel[jj];}
			}
		}
		return(theRes);
	}
	

}