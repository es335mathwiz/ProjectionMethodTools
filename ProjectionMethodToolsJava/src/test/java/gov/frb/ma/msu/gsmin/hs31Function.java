package gov.frb.ma.msu.gsmin;
import gov.frb.ma.msu.ProjectionTools.VectorFunction;
import gov.frb.ma.msu.gsmin.FAndG;
import gov.frb.ma.msu.gsmin.ValueDerivative;
import Jama.Matrix;
public class hs31Function extends FAndG{



	

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
		theRes[0][0]=9*java.lang.Math.pow(xx.get(0,0),2) + java.lang.Math.pow(xx.get(1,0),2) + 9*java.lang.Math.pow(xx.get(2, 0),2);
		double [][]theDrvRes =new double[3][1];
		theDrvRes[0][0]=18*xx.get(0,0);
		theDrvRes[1][0]=2*xx.get(1,0);
		theDrvRes[2][0]=18*xx.get(2,0);
		return(new ValueDerivative(theRes,theDrvRes));};}
			;
		
				
				VectorFunction gg = new VectorFunction(){
			public ValueDerivative evaluate(Matrix xx){
				double [][]theRes =new double[1][7];
				theRes[0][0]=1-(xx.get(0,0)*xx.get(1,0));
				theRes[0][1]=xx.get(0,0)-10;
				theRes[0][2]=-xx.get(0,0)-10;
				theRes[0][3]=xx.get(1,0)-10;
				theRes[0][4]=-xx.get(1,0)+1;
				theRes[0][5]=xx.get(2, 0)-1;
				theRes[0][6]=-xx.get(2, 0)-10;
				double [][]theDrvRes =new double[3][7];
				theDrvRes[0][0]=-xx.get(1,0);
				theDrvRes[1][0]=-xx.get(0,0);
				theDrvRes[2][0]=0;
				
				theDrvRes[0][1]=1;
				theDrvRes[1][1]=0;
				theDrvRes[2][1]=0;
				
				theDrvRes[0][2]=-1;
				theDrvRes[1][2]=0;
				theDrvRes[2][2]=0;
				
				theDrvRes[0][3]=0;
				theDrvRes[1][3]=1;
				theDrvRes[2][3]=0;
				

				theDrvRes[0][4]=0;
				theDrvRes[1][4]=-1;
				theDrvRes[2][4]=0;
				
				theDrvRes[0][5]=0;
				theDrvRes[1][5]=0;
				theDrvRes[2][5]=1;
				
				theDrvRes[0][6]=0;
				theDrvRes[1][6]=0;
				theDrvRes[2][6]=-1;
			return(new ValueDerivative(theRes,theDrvRes));};}
					;/*
hs31FuncParams[] :=
    funcParams[Function[{x1,x2,x3},9*x1^2+x2^2+9*x3^2],
    Function[{x1,x2,x3},{1-x1*x2,x1-10,-x1-10,x2-10,-x2+1,x3-1,-x3-10}]]
 */
		
			
			
		
		


	}
	/*
	vectorFunction fFunc, vectorFunction fDrvFunc, vectorFunction gFunc, vectorFunction gDrvFunc
	*/
