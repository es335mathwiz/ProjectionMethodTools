package gov.frb.ma.msu.gsmin;
import Jama.Matrix;
public abstract class FAndG {
abstract ValueDerivative evaluateF(Matrix xx);
abstract ValueDerivative evaluateG(Matrix xx);
public Matrix vec(Matrix rect){
int numRows=rect.getRowDimension();
	int numCols=rect.getColumnDimension();
	return(new Matrix(rect.getColumnPackedCopy(),numRows*numCols));
}
public Matrix unVec(Matrix vec,int numRows){
	return(new Matrix(vec.getColumnPackedCopy(),numRows));
}
}

