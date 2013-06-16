package gov.frb.ma.msu.ProjectionMethodToolsJava;

public class NewtonIterInfo {
	double [][] xx;
	double [][] delta;
	double lambda;
	double [][] fVal;
	boolean convergedQ=false;
	double shrinkFactorNow;
	public double getShrinkFactorNow() {
		return shrinkFactorNow;
	}

	public void setShrinkFactorNow(double shrinkFactorNow) {
		this.shrinkFactorNow = shrinkFactorNow;
	}

	public boolean isConvergedQ() {
		return convergedQ;
	}
	
	public void setConvergedQ(boolean convergedQ) {
		this.convergedQ = convergedQ;
	}
	
	NewtonIterInfo(double [][]xval,double [][] dVal, double lamVal,double [][] fNow, boolean hasConverged){
		xx=new double[xval.length][xval[0].length];
		twoDimArrayCopy(xval,xx);
		delta=new double[dVal.length][dVal[0].length];
		twoDimArrayCopy(dVal,delta);
		lambda=lamVal;
		fVal=new double[fNow.length][fNow[0].length];
		twoDimArrayCopy(fNow, fVal);
		setConvergedQ(hasConverged);
	}
	
	void twoDimArrayCopy(double [][] source,double [][] copy){
		int ii,cols=source[0].length;
		for(ii=0;ii<source.length;ii++){
			System.arraycopy(source[ii],0,copy[ii],0,cols);
		}
	}
	
	public double[][] getXx() {
		return xx;
	}
	
	public void setXx(double[][] xx) {
		this.xx = xx;
	}
	
	public double[][] getDelta() {
		return delta;
	}
	
	public void setDelta(double[][] delta) {
		this.delta = delta;
	}
	
	public double getLambda() {
		return lambda;
	}
	
	public void setLambda(double lambda) {
		this.lambda = lambda;
	}
	
	public double[][] getfVal() {
		return fVal;
	}
	
	public void setfVal(double[][] fVal) {
		this.fVal = fVal;
	}

}
