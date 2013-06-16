package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class Evaluater {

	private double [] params;

	public Evaluater() {
		super();
	}

	public EquationValDrv makeEVD(int indx, int size, double theVal) {
		Matrix theV=new Matrix(1,1,theVal);
		Matrix theJ=new Matrix(1,size);
		theJ.set(0, indx-1, 1);
		return(new EquationValDrv(theV,theJ));
	}



	public double[] getParams() {
		return params;
	}
	public void setParams(double[] params) {
		this.params = params;
	}
}