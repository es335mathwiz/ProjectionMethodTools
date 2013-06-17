package gov.frb.ma.msu.ProjectionMethodToolsJava;
import no.uib.cipr.matrix.sparse.*;
import no.uib.cipr.matrix.io.*;

import no.uib.cipr.matrix.*;
import junit.framework.TestCase;
import java.io.*;
/**
 * @author m1gsa00
 *
 */


public class mtjTests extends TestCase {

	/**
	 * @param name
	 */
	
	FlexCompRowMatrix crMat;
	FlexCompRowMatrix crMatNot;
	SparseVector sv;
	public mtjTests(String name) {
		super(name);
	}
	
public void testCreateSparse(){
	/*add two matrices*/
    FlexCompRowMatrix crMat00= new FlexCompRowMatrix(crMat.add(crMatNot));
    /*access a matrix element*/
    double nada=crMat.get(0,2);
    /*vector multplied by a matrix*/
    SparseVector crMat01= new SparseVector(crMat.mult(sv, new SparseVector(6)));
    int [][] nzNow ={{0},{},{},{},{},{}};
    /*matrix multiplied by a matrix*/
    FlexCompRowMatrix nVal= new FlexCompRowMatrix(crMat.mult(crMat,new FlexCompRowMatrix(6,6)));
    Matrix aMat = crMat00.mult(crMat00, crMat.copy());
}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
	// http://web.eecs.utk.edu/~dongarra/etemplates/node373.html	
		int[][] nz ={{0,4},{0,1,5},{1,2,3},{0,2,3,4},{1,3,4,5},{1,4,5}};
		crMat=new FlexCompRowMatrix(6, 6);
		crMat.set(0, 0, 10.);
		crMat.set(0, 4, -2.);
		
		crMat.set(1, 0, 3.);
		crMat.set(1, 1, 9.);
		crMat.set(1, 5, 3.);
		
		crMat.set(2, 1, 7);
		crMat.set(2, 2, 8);
		crMat.set(2, 3, 7);
		
		crMat.set(3, 0, 3);
		crMat.set(3, 2, 8);
		crMat.set(3, 3, 7);
		crMat.set(3, 4, 5);
		
		crMat.set(4, 1, 8);
		crMat.set(4, 3, 9);
		crMat.set(4, 4, 9);
		crMat.set(4, 5, 13);
		
		crMat.set(5, 1, 4);
		crMat.set(5, 4, 2);
		crMat.set(5, 5, -1);
		int[][] nz01 ={{1,5},{1,2,5},{},{},{},{}};
		crMatNot=new FlexCompRowMatrix(6, 6);
		crMatNot.set(0, 1, 10.);
		crMatNot.set(0, 5, -2.);
		
		crMatNot.set(1, 1, 3.);
		crMatNot.set(1, 2, 9.);
		crMatNot.set(1, 5, 3.);
		
		
//		int[] svNz={1,3,5}; double[] svVals={4,3,7};
		int[] svNz={3}; double[] svVals={3};
		sv=new SparseVector(6,svNz,svVals);
		// http://math.nist.gov/MatrixMarket/searchtool.html
//		File dir1 = new File (".");
//	       System.out.println ("pwd : " + dir1.getCanonicalPath());

		FileReader fr = new FileReader("src/test/java/gov/frb/ma/msu/mcca.mtx");
    MatrixVectorReader mr = new MatrixVectorReader(fr);
    CompRowMatrix notmtta=new CompRowMatrix(mr);
    FlexCompRowMatrix  mtta = new FlexCompRowMatrix(notmtta);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
}
