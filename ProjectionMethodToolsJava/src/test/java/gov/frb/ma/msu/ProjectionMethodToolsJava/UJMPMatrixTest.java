package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjCodeMatrix;
import gov.frb.ma.msu.ProjectionMethodToolsJava.UJMPMatrix;

import org.ujmp.core.Matrix;
import org.ujmp.core.doublematrix.impl.DefaultSparseDoubleMatrix;
import org.ujmp.core.*;

import junit.framework.TestCase;

public class UJMPMatrixTest extends TestCase {
	
	ProjCodeMatrix matrix1 = new ProjCodeMatrix();
	ProjCodeMatrix matrix2 = new ProjCodeMatrix();
	UJMPMatrix ujmpmatrix = new UJMPMatrix();

	public void testAdd(){ 
		double[][] C1 = {{0,0,0},{0,2,4},{8,0,0}}; 
		Matrix m1 = SparseMatrix.Factory.zeros(3,3); 
		//Matrix m1 = DefaultSparseDoubleMatrix.Factory.zeros(3,3);
		m1.setAsDouble(2,1,1);
		m1.setAsDouble(3,2,0); 
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(m1);
		Matrix m2 = SparseMatrix.Factory.zeros(3,3);
		m2.setAsDouble(5,2,0);
		m2.setAsDouble(4,1,2);
		matrix2.UJMPRepresentation = new DefaultSparseDoubleMatrix(m2);
		
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.add(matrix1, matrix2);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}
				
				MyAssert.assertArrayEquals(C1, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	public void testAddScalar(){ 
		double[][] C1 = {{0,0,0},{0,7,0},{8,0,0}}; 
		Matrix m1 = SparseMatrix.Factory.zeros(3,3); 
		m1.setAsDouble(2,1,1);
		m1.setAsDouble(3,2,0); 
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(m1);		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.addScalar(matrix1, 5);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}			
/*				for(int ii=0;ii<result.length;ii++){
					for(int jj=0;jj<result[0].length;jj++){
						System.out.print(result[ii][jj]+" ");
					}
					System.out.print("\n");
				}*/			
				MyAssert.assertArrayEquals(C1, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	public void testsubtractScalar(){ 
		double[][] C1 = {{0,0,0},{0,-3,0},{-2,0,0}}; 
		Matrix m1 = SparseMatrix.Factory.zeros(3,3); 
		m1.setAsDouble(2,1,1);
		m1.setAsDouble(3,2,0); 
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(m1);		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.subtractScalar(matrix1, 5);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}			
/*				for(int ii=0;ii<result.length;ii++){
					for(int jj=0;jj<result[0].length;jj++){
						System.out.print(result[ii][jj]+" ");
					}
					System.out.print("\n");
				}*/			
				MyAssert.assertArrayEquals(C1, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	public void testMult(){  
		double[][] C2 = {{28,0,0},{0,6,12},{7,0,0}};
		Matrix A = SparseMatrix.Factory.zeros(3,3);
		A.setAsDouble(4,0,2);
		A.setAsDouble(6,1,0);
		A.setAsDouble(1,2,2);
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(A);
		
		Matrix B = SparseMatrix.Factory.zeros(3,3);
		B.setAsDouble(1,0,1);
		B.setAsDouble(2,0,2);
		B.setAsDouble(7,2,0);
		matrix2.UJMPRepresentation = new DefaultSparseDoubleMatrix(B);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.times(matrix1, matrix2);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}
				
				MyAssert.assertArrayEquals(C2, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	
	public void testMultiScalar(){ 
		double[][] C3 = {{0,0,12},{18,0,0},{0,0,3}};
		Matrix A = SparseMatrix.Factory.zeros(3,3);
		A.setAsDouble(4,0,2);
		A.setAsDouble(6,1,0);
		A.setAsDouble(1,2,2);
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(A);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.timesScalar(matrix1, 3);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){ //Can only set up a 2-Dim. Matrix
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}
/*				for(int ii=0;ii<result.length;ii++){
					for(int jj=0;jj<result[0].length;jj++){
						System.out.print(result[ii][jj]+" ");
					}
					System.out.print("\n");
				}*/
				
				
				MyAssert.assertArrayEquals(C3, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	public void testExp(){
		double[][] C3 = {{0,0,0},{0,0,0},{0,0,2.718}};
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(SparseMatrix.Factory.zeros(3,3));
		matrix1.UJMPRepresentation.setAsDouble(1,2,2);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.expE(matrix1);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){ //Can only set up a 2-Dim. Matrix
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}	
				MyAssert.assertArrayEquals(C3, result, 1.0e-3);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	public void testPowE(){
		double[][] C4 = {{0,0,0},{0,0,0},{0,0,4}};
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(SparseMatrix.Factory.zeros(3,3));
		matrix1.UJMPRepresentation.setAsDouble(2,2,2);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.powE(matrix1, 2);
				double[][] result1 = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){ //Can only set up a 2-Dim. Matrix
					int row = (int) pos[0];
					int col = (int) pos[1];
					result1[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}
				MyAssert.assertArrayEquals(C4, result1, 1.0e-10); //never through an error when incorrect calculation!!! FAIL SOMETHINGS NOT RIGHT	
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	public void testPow(){  
		double[][] C2 = {{0,0,4},{0,0,24},{0,0,1}};
		Matrix A = SparseMatrix.Factory.zeros(3,3);
		A.setAsDouble(4,0,2);
		A.setAsDouble(6,1,0);
		A.setAsDouble(1,2,2);
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(A);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.pow(matrix1, 3);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}
				
				MyAssert.assertArrayEquals(C2, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}
	
	
	public void testLogE(){
		double[][] C3 = {{0,0,0},{0,0,0},{0,0,0}};
		matrix1.UJMPRepresentation = new DefaultSparseDoubleMatrix(SparseMatrix.Factory.zeros(3,3));
		matrix1.UJMPRepresentation.setAsDouble(1,2,2);
		
		try {    
				ProjCodeMatrix theRes = ujmpmatrix.logE(matrix1);
				double[][] result = new double[(int) theRes.UJMPRepresentation.getRowCount()][(int) theRes.UJMPRepresentation.getColumnCount()]; // initialize to all zeros
				for(long[] pos : theRes.UJMPRepresentation.availableCoordinates()){ //Can only set up a 2-Dim. Matrix
					int row = (int) pos[0];
					int col = (int) pos[1];
					result[row][col] = theRes.UJMPRepresentation.getAsDouble(pos);	
				}	
			
				MyAssert.assertArrayEquals(C3, result, 0);
			
		} catch (Exception ee){
			ee.printStackTrace();
		}
	}

	
}
