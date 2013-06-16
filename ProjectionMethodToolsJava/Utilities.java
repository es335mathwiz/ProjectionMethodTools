package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;

public class Utilities {
 
	public static double[] augmentVecWithVal(double[] vec, double[] val) {
		double[] theRes= new double[vec.length+val.length];
		System.arraycopy(vec, 0, theRes, 0, vec.length);
		System.arraycopy(val, 0, theRes, vec.length, val.length);
		return theRes;
	}

public	static double[][] augmentMatrixWithVal(double[][] mat, double[] val) {
				double[][] theRes= new double[mat.length][mat[0].length+val.length];
		int ii;
		for(ii=0;ii<mat.length;ii++){
			theRes[ii]=   augmentVecWithVal(mat[ii],val);
		}
		return theRes;
	}

/**
 * places the ijth value of newVals in the (polyNum, jth) location of matrix i
 * @param polyNum int
 * @param newVals double [][]
 * @param target
 */
public static void placeValuesNodewise(int polyNum, double[][] newVals, Matrix[] target) {
	int numNodes=newVals.length;
	int numPolys=newVals[0].length;
	int ii,jj;
	for(ii=0;ii<numNodes;ii++){
		for (jj=0;jj<numPolys;jj++){
			target[ii].set(polyNum, jj, newVals[ii][jj]);
		}
	}
}

/**
 * Checks the two dimensional matrix for NaNs. If none are found the method 
 * silently returns.  If a NaN is found the method uses the String argument
 * to print a message to standard out and throws a
 * ProjectionRuntimeException.
 * 
 * @param xx double[][]
 * @param theVar String 
 * @throws ProjectionRuntimeException
 */
public static void chkNan(double[][] xx, String theVar)
		throws ProjectionRuntimeException {
		int ii;int jj;
		String eStr;
		for(ii=0;ii<xx.length;ii++){
		for(jj=0;jj<xx[0].length;jj++){
		    if(Double.isNaN(xx[ii][jj])) {
			eStr="CollocationSolution.root: NaN in computation at "+theVar+"["
			    +ii+"]["+jj+"]";
			throw new ProjectionRuntimeException(eStr);}
		
		}}
		
		}

public static double [][] nxtWts(double [][] prevWts, int [] oldMaxPows, int[] newMaxPows) {
	int numRows=prevWts.length;int numCols=1;int ii,jj;int numPows=newMaxPows.length;
	int numOldCols=prevWts[0].length;
	int [] indices = Utilities.theIndices(oldMaxPows,newMaxPows);
	for(ii=0;ii<numPows;ii++){
		numCols=numCols*(1+newMaxPows[ii]);
	}
	double[][] newWts = new double[numRows][numCols];
	for(ii=0;ii<numRows;ii++){
		for(jj=0;jj<numOldCols;jj++){
			newWts[ii][indices[jj]-1]=prevWts[ii][jj];
		}
	}
	return(newWts);
}

public static int [] theIndices(int [] oldMaxPows, int [] newMaxPows) {
	int [] [] allP=StochasticBasis.allPowers(oldMaxPows);int numAll=allP.length;int ii;
	int [] theRes =new int[numAll];
	for(ii=0;ii<numAll;ii++){
		theRes[ii]=StochasticBasis.indexPower(allP[ii],0,newMaxPows);
	}
	return(theRes);
}

/**
 * Copy the component of the two dimensional state vector used in the newton iteration 
 * to the matrix characterizing the weights for the non-state variable 
 * polynomials
 * @param xx matrix of weights for polynomials used by newton for solving collocation equation system
 * @param stateVarDim the number of state variables
 * @param nonStateVarDim the number of non-state variables
 * @param nodeDim the number of collocation points
 * @param nonStateXX matrix of weights for nonstate polynomials
 */

	 static void copyXxToNonState(double[][] xx, int stateVarDim, int nonStateVarDim, int nodeDim, double[][] nonStateXX) {
		int ii;
		int jj;
		for(ii=0;ii<nonStateVarDim;ii++){
		for(jj=0;jj<nodeDim;jj++){
		    nonStateXX[ii][jj]=xx[stateVarDim+ii][jj];
		}}
	}

/**
	  * Copy the component of the two dimensional state vector used in the newton iteration 
	  * to the matrix characterizing the weights for the-state variable 
	  * polynomials
	  * @param xx matrix of weights for polynomials used by newton for solving collocation equation system
	  * @param stateVarDim the number of state variables
	  * @param stateVarDim the number of state variables
	  * @param nodeDim the number of collocation points
	  * @param stateXX matrix of weights for nonstate polynomials
	  */
 static void copyXxToState(double[][] xx, int stateVarDim, int nodeDim, double[][] stateXX) {
		int ii;
		int jj;
		for(ii=0;ii<stateVarDim;ii++){
		for(jj=0;jj<nodeDim;jj++){
		    stateXX[ii][jj]=xx[ii][jj];
		}}
	}

}
