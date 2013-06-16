package gov.frb.ma.msu.ProjectionMethodToolsJava;
import Jama.Matrix;


/**
 * 
 * @author m1crg00
 * JamaMatrix.java implements the Jama Matrix operation package for MatrixStrategy.
 * For the sake of computational time, JamaMatrix operations should be used only for
 * dense matrix calculations.
 */
public class JamaMatrix implements MatrixStrategy {
	
	ProjCodeMatrix theRes;
	
	/**
	 * Performs addition operation on dense matrices, C = A + B
	 * @see Jama.Matrix.plus()
	 * 
	 * @param aa - Matrix
	 * @param bb - Matrix
	 * @return A+B
	 */
	
	public ProjCodeMatrix add(ProjCodeMatrix aa, ProjCodeMatrix bb) { //must return type ProjCodeMatrix
		theRes.JamaRepresentation = aa.JamaRepresentation.plus(bb.JamaRepresentation);
		return theRes;
	}
	
	/**
	 * Performs scalar addition operation on dense matrices, C = A.+b
	 * 
	 * @param aa - Matrix
	 * @param bb - double 
	 * @return A.+b
	 */
	
	public ProjCodeMatrix addScalar(ProjCodeMatrix aa, double bb) {
		double[][] theAA = aa.JamaRepresentation.getArrayCopy();
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		for(int ii=0;ii<numRows;ii++){
			for(int jj=0;jj<numCols;jj++){
				theAA[ii][jj] = theAA[ii][jj] + bb;
			}
		}
		theRes.JamaRepresentation = new Matrix(theAA);
		return theRes;
	}

	/**
	 * Performs subtraction operation on dense matrices, C = A-B
	 * @see Jama.Matrix.minus()
	 * 
	 * @param aa - Matrix
	 * @param bb - Matrix 
	 * @return A-B
	 */
	
	public ProjCodeMatrix subtract(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.JamaRepresentation = aa.JamaRepresentation.minus(bb.JamaRepresentation);
		return theRes;
	}
	
	/**
	 * Performs scalar subtraction operation on dense matrices, C = A.-b
	 * 
	 * @param aa - Matrix
	 * @param bb - double 
	 * @return A.-b
	 */
	
	public ProjCodeMatrix subtractScalar(ProjCodeMatrix aa, double bb) {
		double[][] theAA = aa.JamaRepresentation.getArrayCopy();
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		for(int ii=0;ii<numRows;ii++){
			for(int jj=0;jj<numCols;jj++){
				theAA[ii][jj] = theAA[ii][jj] - bb;
			}
		}
		theRes.JamaRepresentation = new Matrix(theAA);
		return theRes;
	}
	
	/**
	 * Performs matrix multiplication operation on dense matrices, C = A*B
	 * @see Jama.Matrix.times(Matrix)
	 * @param aa - Matrix
	 * @param bb - Matrix
	 * @return A*B
	 */
	
	public ProjCodeMatrix times(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.JamaRepresentation = aa.JamaRepresentation.times(bb.JamaRepresentation);
		return theRes;
	}
	
	/**
	 * Performs element multiplication for two matrices 
	 * @see 
	 * @param aa - Matrix
	 * @param bb - Matrix
	 * @return A(ii,jj)*B(ii,jj) for all ii,jj
	 */
	
	@Override
	public ProjCodeMatrix timesE(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.JamaRepresentation = aa.JamaRepresentation.arrayTimes(bb.JamaRepresentation);
		return theRes;
	}
	
	/**
	 * Performs scalar multiplication operation on dense matrices, C = A.*b
	 * @see Jama.Matrix.times(double)
	 * @param aa - Matrix
	 * @param bb - double 
	 * @return A.*b
	 */
	
	public ProjCodeMatrix timesScalar(ProjCodeMatrix aa, double bb) {
//		theRes.JamaRepresentation = aa.JamaRepresentation.times(bb);
		return new ProjCodeMatrix(aa.JamaRepresentation.times(bb));
	}
	

	/**
	 * Performs right matrix division operation on dense matrices, C = A./B
	 * 
	 * @param aa - Matrix 
	 * @param bb - Matrix
	 * @return A./B
	 */
	public ProjCodeMatrix divide(ProjCodeMatrix aa, ProjCodeMatrix bb) { //right division
		theRes.JamaRepresentation = aa.JamaRepresentation.times(bb.JamaRepresentation.inverse());
		return theRes;
	}

	/**
	 * Performs elemental exponentials on a dense matrix, C(ii,jj) = exp(A(ii,jj))
	 * 
	 * @param A - Matrix
	 * @return exp(A(ii,jj)) for every entry A(ii,jj) in matrix A
	 */
	public ProjCodeMatrix expE(ProjCodeMatrix aa) {   // element-by-element exponential
		double [][] expAA = aa.JamaRepresentation.getArrayCopy();
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		for(int ii = 0; ii<numRows; ii++){
			for(int jj=0; jj<numCols; jj++){
				expAA[ii][jj] = java.lang.Math.exp(expAA[ii][jj]);
			}
		}
		theRes.JamaRepresentation = new Matrix(expAA);
		return theRes;
	}
	
	/**
	 * Performs Matrix power operation (ex. A^3 = A * A * A)
	 * 
	 * @param A - Matrix
	 * @param b (exponent) - Integer
	 * @return A^b
	 */
	public ProjCodeMatrix pow(ProjCodeMatrix aa, int exponent) { // Matrix power implementation
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		Matrix I = Matrix.identity(numRows, numCols);
		theRes.JamaRepresentation = I;
		if ( exponent > 0 ){
			for(int ii=0; ii<exponent; ii++){
				theRes.JamaRepresentation = theRes.JamaRepresentation.times(aa.JamaRepresentation);
			}
		}
		return theRes;
	}
	/**
	 * Performs elemental powers (ex. C(ii,jj) = A(ii,jj)^b
	 * 
	 * @param A - Matrix
	 * @param b (exponent) - int
	 * @return A(ii,jj)^b for all elements in A
	 */
	public ProjCodeMatrix powE(ProjCodeMatrix aa, int exponent){  // element-by-element power
		double [][] powAA = aa.JamaRepresentation.getArrayCopy();
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		for(int ii = 0; ii<numRows; ii++){
			for(int jj=0; jj<numCols; jj++){
				powAA[ii][jj] = java.lang.Math.pow(powAA[ii][jj],exponent);
			}
		}
		theRes.JamaRepresentation = new Matrix(powAA);
		return theRes;
	}
	/**
	 * Performs natural log function on all elements within a matrix
	 * 
	 * @param A - Matrix
	 * @return log(A(ii,jj)) for all elements in A
	 */
	public ProjCodeMatrix logE(ProjCodeMatrix aa){
		double[][] LogAA = aa.JamaRepresentation.getArrayCopy();
		int numRows = aa.JamaRepresentation.getRowDimension();
		int numCols = aa.JamaRepresentation.getColumnDimension();
		for(int ii = 0; ii<numRows; ii++){
			for(int jj=0; jj<numCols; jj++){
				LogAA[ii][jj] = java.lang.Math.log(LogAA[ii][jj]);
			}
		}
		theRes.JamaRepresentation = new Matrix(LogAA);
		return theRes;
	}
	
	public ProjCodeMatrix fromArray(double[][] array){
		theRes.JamaRepresentation = new Matrix(array);
		return theRes;
	}

	public double[][] getArray(ProjCodeMatrix aa) {
		double[][] array = aa.JamaRepresentation.getArray();
		return array;
	}

}
