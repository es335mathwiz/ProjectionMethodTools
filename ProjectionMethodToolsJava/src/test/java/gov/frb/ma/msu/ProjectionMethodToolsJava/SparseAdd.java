package gov.frb.ma.msu.ProjectionMethodToolsJava;

import no.uib.cipr.matrix.sparse.FlexCompRowMatrix;

public class SparseAdd {
	public static void main(String[] args){
	int[][] nz0 ={{1},{0}};
	FlexCompRowMatrix crMat0=new FlexCompRowMatrix(2, 2);
	crMat0.set(0,1,10.);
	crMat0.set(1,0,-2.);
	int[][] nz1 ={{0},{1}};
	FlexCompRowMatrix crMat1=new FlexCompRowMatrix(2, 2);
	crMat1.set(1,0,10.);
	crMat1.set(0,1,-2.);
	FlexCompRowMatrix crMat2=(FlexCompRowMatrix) crMat0.add(crMat1);

}
}