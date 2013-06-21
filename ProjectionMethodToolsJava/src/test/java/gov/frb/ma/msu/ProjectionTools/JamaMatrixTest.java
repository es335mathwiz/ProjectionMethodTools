package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.JamaMatrix;
import gov.frb.ma.msu.ProjectionTools.ProjCodeMatrix;
import Jama.Matrix;
import junit.framework.TestCase;

public class JamaMatrixTest extends TestCase{
	
	double[][] A = {{1.,1.,1.},{1.,1.,1.},{1.,1.,1.}};
	double[][] B = {{2.,2.,2.},{2.,2.,2.},{2.,2.,2.}};
	ProjCodeMatrix matrix1 = new ProjCodeMatrix();
	ProjCodeMatrix matrix2 = new ProjCodeMatrix();
	Jama.Matrix JamaRep;
//	MatrixStrategy aa = (MatrixStrategy) JamaRep; // no!!!!!!!

	public void testAdd(){
		double[][] C1 = {{3.,3.,3.},{3.,3.,3.},{3.,3.,3.}};
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(A);
		matrix2.JamaRepresentation = new Matrix(B);
		try {    
			ProjCodeMatrix theRes = jamamatrix.add(matrix1, matrix2);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C1, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	public void testMult(){
		double[][] C2 = {{6.,6.,6.},{6.,6.,6.},{6.,6.,6.}};	
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(A);
		matrix2.JamaRepresentation = new Matrix(B);
		try {    
			ProjCodeMatrix theRes = jamamatrix.times(matrix1, matrix2);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C2, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}	
	}

	public void testMultSCALAR(){
		double[][] C3 = {{6.,6.,6.},{6.,6.,6.},{6.,6.,6.}};	
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(A);
		try {    
			ProjCodeMatrix theRes = jamamatrix.timesScalar(matrix1, 6);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C3, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}	
	}
	
	public void testExp(){
		double[][] C4 = {{2.718,2.718,2.718},{2.718,2.718,2.718},{2.718,2.718,2.718}};	
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(A);
		try {    
			ProjCodeMatrix theRes = jamamatrix.expE(matrix1);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C4, 1.0e-3);
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
		
	public void testPow(){
		double[][] C5 = {{4.,4.,4.},{4.,4.,4.},{4.,4.,4.}};	
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(B);
		try {    
			ProjCodeMatrix theRes = jamamatrix.powE(matrix1, 2);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C5, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
		
	public void testLog(){
		double[][] C6 = {{0.,0.,0.},{0.,0.,0.},{0.,0.,0.}};	
		JamaMatrix jamamatrix = new JamaMatrix();
		matrix1.JamaRepresentation = new Matrix(A);
		try {    
			ProjCodeMatrix theRes = jamamatrix.logE(matrix1);
			double[][] test = theRes.JamaRepresentation.getArrayCopy();
			MyAssert.assertArrayEquals(test, C6, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}

	public void testAll(){
		double[][] C6 = {{0.,0.,0.},{0.,0.,0.},{0.,0.,0.}};	
		//MatrixStrategy a; //This needs to be the determining factor
		double one = 0.0;
		MatrixStrategy theMS = (MatrixStrategy) new JamaMatrix(); // no!!!!!!!

//		MatrixContext matrixStrategy = new MatrixContext(aa); 
		Jama.Matrix theJM=new Jama.Matrix(C6);
		ProjCodeMatrix test = new ProjCodeMatrix(theJM);
//		test.JamaRepresentation = new Matrix(A);
		ProjCodeMatrix result = theMS.timesScalar(test,one);
		try {    
			double[][] array = result.JamaRepresentation.getArrayCopy();
			for(int ii=0;ii<array.length;ii++){
				for(int jj=0;jj<array[0].length;jj++){
					System.out.print(array[ii][jj]+" ");
				}
				System.out.print("\n");
			}	
			MyAssert.assertArrayEquals(array, C6, 0);
		} catch (Exception ee){
			ee.printStackTrace();
		}
	
		
	}
	
}
