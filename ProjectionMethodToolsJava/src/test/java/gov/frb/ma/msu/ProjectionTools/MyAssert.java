package gov.frb.ma.msu.ProjectionTools;
import junit.framework.Assert;


public class MyAssert extends Assert {
static	int ii,jj,numRows,numCols;
	static void assertArrayEquals(double [][] expRes,double [][] actRes,double tol) {
numRows=expRes.length;numCols=expRes[0].length;
//System.out.println("\nbegin\n{");
		for(ii=0;ii<numRows;ii++){

			for(jj=0;jj<numCols;jj++){
//				System.out.println(actRes[ii][jj]+",");
				assertEquals(expRes[ii][jj],actRes[ii][jj],tol);
//				System.out.flush();
			}
		}
//		System.out.println("}\nend\n");
	}
	
	static void assertArrayEquals(double [] expRes,double [] actRes,double tol) {
numRows=expRes.length;
		for(ii=0;ii<numRows;ii++){
				assertEquals(expRes[ii],actRes[ii],tol);
			}
		
	}
}
