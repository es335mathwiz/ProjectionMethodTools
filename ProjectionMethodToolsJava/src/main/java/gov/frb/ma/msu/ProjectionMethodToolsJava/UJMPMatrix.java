package gov.frb.ma.msu.ProjectionMethodToolsJava;

import org.ujmp.core.Matrix;
import org.ujmp.core.calculation.Calculation.Ret;
import org.ujmp.core.doublematrix.impl.DefaultSparseDoubleMatrix;
import org.ujmp.core.matrix.SparseMatrix;

/**
 * 
 * @author m1crg00
 * This class implements the UJMP package under MatrixStrategy. 
 * Here operations use the UJMP package to do sparse matrix operations.
 *
 */
public class UJMPMatrix implements MatrixStrategy {
	
	ProjCodeMatrix theRes;
/**
 * 	Performs Matrix (elemental) addition on a two sparse matrices A and B
 * @param A - Sparse Matrix
 * @param B - Sparse Matrix
 * @return A+B
 */
	public ProjCodeMatrix add(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.plus(bb.UJMPRepresentation);
		return theRes;
	}
/**
 * Performs elemental addition of a sparse matrix A and a scalar b
 * @param A - Sparse Matrix
 * @param b - double
 * @return A(ii,jj)+b for all non-zero elements in sparse matrix A
 */
	public ProjCodeMatrix addScalar(ProjCodeMatrix aa, double bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.plus(bb);
		return theRes;
	}
	/**
	 * Performs matrix subtraction on two sparse matrices A and B
	 * @param A - Sparse Matrix
	 * @param B - Sparse Matrix
	 * @return A - B
	 */
	public ProjCodeMatrix subtract(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.minus(bb.UJMPRepresentation);
		return theRes;
	}
	/**
	 * Performs elemental subtraction of a sparse matrix A and a scalar b
	 * @param A - Sparse Matrix
	 * @param b - double
	 * @return A(ii,jj)-b for all non-zero elements in sparse matrix A
	 */
	public ProjCodeMatrix subtractScalar(ProjCodeMatrix aa, double bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.minus(bb);
		return theRes;
	}
	/**
	 * Performs matrix multiplication on two sparse matrices
	 * @param A - Sparse Matrix
	 * @param B - Sparse Matrix
	 * @return A*B
	 */
	public ProjCodeMatrix times(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.mtimes(bb.UJMPRepresentation);
		return theRes;
	}
	/**
	 * Performs elemental multiplication on a sparse matrix
	 * @param A - Sparse Matrix
	 * @param B - Sparse Matrix
	 * @returns A(ii,jj)*B(ii,jj) for all elements in A and B
	 */
	public ProjCodeMatrix timesE(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.times(bb.UJMPRepresentation); 
		return theRes;
	}
	/**
	 * Performs elemental multiplication on a sparse matrix
	 * @param A - Sparse Matrix
	 * @param b - double
	 * @returns A(ii,jj)*b for all elements in A
	 */
	public ProjCodeMatrix timesScalar(ProjCodeMatrix aa, double bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.times(bb);
		return theRes;
	}
	/**
	 * Performs right hand division on two sparse matrices
	 * @param A - Sparse Matrix
	 * @param B - Sparse Matrix
	 * @returns A./B
	 */
	public ProjCodeMatrix divide(ProjCodeMatrix aa, ProjCodeMatrix bb) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.divide(bb.UJMPRepresentation);
		return theRes;
	}
	/**
	 * Performs elemental exponential operation on a sparse matrix
	 * @param A - Sparse Matrix
	 * @returns exp(A(ii,jj)) for all non-zero elements in A
	 */
	public ProjCodeMatrix expE(ProjCodeMatrix aa){
		theRes.UJMPRepresentation = aa.UJMPRepresentation;
		for(long[] pos : aa.UJMPRepresentation.availableCoordinates()){
			theRes.UJMPRepresentation.setAsDouble(java.lang.Math.exp(aa.UJMPRepresentation.getAsDouble(pos)),pos[0],pos[1]);
		}
		return theRes;
	}
	/**
	 * Performs elemental power operation on a sparse matrix
	 * @param A - Sparse Matrix
	 * @param b (exponent) - integer
	 * @returns A^b = A * A * ... * A
	 */
	public ProjCodeMatrix pow(ProjCodeMatrix aa, int exponent) {
		if(exponent == 1){
			theRes.UJMPRepresentation = aa.UJMPRepresentation;
		} else if(exponent > 1){
			Matrix MatPow = aa.UJMPRepresentation;
			for(int jj=0; jj<exponent; jj++){
				MatPow = MatPow.mtimes(aa.UJMPRepresentation);
			}
			theRes.UJMPRepresentation = MatPow;
		} else {
			System.out.println("ERROR: exponent is negative for UJMP pow() implementation");
			// throw exception here
		}
		return theRes;
	}
	/**
	 * Performs elemental power operation on a sparse matrix
	 * @param A - Sparse Matrix
	 * @param b - double
	 * @returns A(ii,jj)*b for all non-zero elements in A
	 */
	public ProjCodeMatrix powE(ProjCodeMatrix aa, int exponent) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation.power(Ret.NEW, exponent);
		return theRes;
	}
	/**
	 * Performs elemental log operation on a sparse matrix
	 * @param A - Sparse Matrix
	 * @returns ln(A(ii,jj)) for all non-zero elements in A
	 */
	public ProjCodeMatrix logE(ProjCodeMatrix aa) {
		theRes.UJMPRepresentation = aa.UJMPRepresentation;
		for(long[] pos : aa.UJMPRepresentation.availableCoordinates()){
			theRes.UJMPRepresentation.setAsDouble(java.lang.Math.log(aa.UJMPRepresentation.getAsDouble(pos)),pos[0],pos[1]);
		}
		return theRes;
	}
	
	public ProjCodeMatrix fromArray(double[][] array){
		int rows = array.length;
		int cols = array[1].length;
		Matrix m1 = SparseMatrix.factory.zeros(rows,cols);
		for(int row=0;row <rows; row++){
			for(int col=0; col<cols; col++){
				if (array[row][col] != 0){
					m1.setAsDouble(array[row][col],row,col);
				}
			}
		}
		theRes.UJMPRepresentation = new DefaultSparseDoubleMatrix(m1);		
		return theRes;
	}

	public double[][] getArray(ProjCodeMatrix aa) {  
		double[][] array = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; 
		for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
			int row = (int) pos[0];
			int col = (int) pos[1];
			array[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
		}					
		return array;
	}

}
