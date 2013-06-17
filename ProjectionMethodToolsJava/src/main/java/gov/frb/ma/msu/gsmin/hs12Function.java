package gov.frb.ma.msu.gsmin;

import gov.frb.ma.msu.ProjectionMethodToolsJava.VectorFunction;
import gov.frb.ma.msu.gsmin.FAndG;
import gov.frb.ma.msu.gsmin.ValueDerivative;
import Jama.Matrix;

public class hs12Function extends FAndG{

	public Matrix xx;
	public int xDim;
	public int gDim;
	public Matrix  fVal;
	public Matrix  fDrvVals;
	public Matrix  gVals;
	public Matrix gDrvVals;

	public ValueDerivative evaluateF(Matrix xx){return(ff.evaluate(xx));}
	public ValueDerivative evaluateG(Matrix xx){return(gg.evaluate(xx));};

	VectorFunction ff = new VectorFunction(){
public ValueDerivative evaluate(Matrix xx){
	double [][]theRes =new double[1][1];
	theRes[0][0]=.5*java.lang.Math.pow(xx.get(0,0),2)+java.lang.Math.pow(xx.get(1,0),2)-xx.get(0,0)*xx.get(1,0)-7*xx.get(0,0)-7*xx.get(1,0);
	double [][]theDrvRes =new double[2][1];
	theDrvRes[0][0]=-7 +  xx.get(0,0)- xx.get(1,0);
	theDrvRes[1][0]=-7 -  xx.get(0,0)+2* xx.get(1,0);
	
	return(new ValueDerivative(theRes,theDrvRes));};}
		;
	
			
			VectorFunction gg = new VectorFunction(){
		public ValueDerivative evaluate(Matrix xx){
			double [][]theRes =new double[1][1];
			theRes[0][0]=-(25. -4*java.lang.Math.pow(xx.get(0,0),2)-java.lang.Math.pow(xx.get(1,0),2));
			double [][]theDrvRes =new double[2][1];
			theDrvRes[0][0]=8*xx.get(0,0);
			theDrvRes[1][0]=2*xx.get(1,0);
		return(new ValueDerivative(theRes,theDrvRes));};}
				;
	
	
	
		
		
	
	


}
/*
vectorFunction fFunc, vectorFunction fDrvFunc, vectorFunction gFunc, vectorFunction gDrvFunc
*/