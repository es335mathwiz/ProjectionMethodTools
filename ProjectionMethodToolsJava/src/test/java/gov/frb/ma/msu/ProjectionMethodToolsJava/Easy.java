package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.Basis;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
import gov.frb.ma.msu.ProjectionMethodToolsJava.RootEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.RootEvaluater;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
public class Easy extends RootEvaluater{

	GridVarSpec d1=new GridVarSpec("d1",0,2);
	GridVarSpec bet=new GridVarSpec("bet",.8,1);
	GridVarSpec delta=new GridVarSpec("delta",0.00,0.04);
	GridVarSpec a1=new GridVarSpec("a1",0,2);
	GridVarSpec[] vsArray={d1,bet,delta,a1};
	GridVars theVars = new GridVars(vsArray);
	GridPointsSpec 	 theGS;
	StateVariablePolynomials sPoly;
	Basis polyBasis;

	public Easy(){
		super();
	}
	public Easy(double[][] theRanges){

	setD1(new GridVarSpec("d1",theRanges[0][0],theRanges[0][1]));
	setBet(new GridVarSpec("bet",theRanges[1][0],theRanges[1][1]));
	setDelta(new GridVarSpec("delta",theRanges[2][0],theRanges[2][1]));
	setA1(new GridVarSpec("a1",theRanges[3][0],theRanges[3][1]));
	GridVarSpec[] anArray={d1,bet,delta,a1};
	setVsArray(anArray);
	setTheVars(new GridVars(vsArray));


	}


public GridVarSpec getD1() {
		return d1;
	}
	public void setD1(GridVarSpec d1) {
		this.d1 = d1;
	}
	public GridVarSpec getBet() {
		return bet;
	}
	public void setBet(GridVarSpec bet) {
		this.bet = bet;
	}
	public GridVarSpec getDelta() {
		return delta;
	}
	public void setDelta(GridVarSpec delta) {
		this.delta = delta;
	}
	public GridVarSpec getA1() {
		return a1;
	}
	public void setA1(GridVarSpec a1) {
		this.a1 = a1;
	}
public 	double [] cmpCoeffs(){
	/*		double[] theRes={
0,-(1/getParams()[1]),-((getParams()[0] + 
         getParams()[1]*(getParams()[0] + getParams()[3]*Math.pow(getParams()[1],3.)*Math.pow(-1 + getParams()[2],2.))*Math.pow(-1 + getParams()[2],2.))/
						(getParams()[1]*getParams()[0]*(-1 + getParams()[2]))),-1		};
	*/	
	/*(p0,p1,p2) {-(p0 p1 p2), p0 p1 + p0 p2 + p1 p2, -p0 - p1 - p2, 1}*/
	/*		double[] theRes={-getParams()[0]*getParams()[1]*getParams()[2],
					 getParams()[0]*getParams()[1]+getParams()[0]*getParams()[2]+getParams()[1]*getParams()[2],
					 -getParams()[0]-getParams()[1]-getParams()[2],
					 1};*/
	/*
In[23]:= CoefficientList[(xx-p0)*(xx-p1)*(xx-7),xx]
Out[23]= {-7 p0 p1, 7 p0 + 7 p1 + p0 p1, -7 - p0 - p1, 1}
	*/
/*	double[] theRes={-7*getParams()[0]*getParams()[1],
					 7*getParams()[0]+7*getParams()[1]+getParams()[0]*getParams()[1],
					 -7-getParams()[0]-getParams()[1],
					 1};*/
	/*(p0,2,1) {-2 p0, 2 + 3 p0, -3 - p0, 1}*/
		double[] theRes={-2*getParams()[0],2+3*getParams()[0],-3 - getParams()[0],1};
	/*	double[] theRes={-2*(getParams()[0]*getParams()[1]*getParams()[0]+5),2+3*(getParams()[0]*getParams()[1]*getParams()[0]+5),-3 - (getParams()[0]*getParams()[1]*getParams()[0]+5),1};*/
	return(theRes);
	}

public int	getNumRoots(){return 3;}
	public EquationValDrv[] genCoeffs(Basis polyBasis){
		return null;
	}

	public double[][] tryIt(int[] theOrds,double[][] initWts){

 	 setTheGS(new GridPointsSpec(theVars,theOrds));
	 sPoly= new StateVariablePolynomials(theGS);
	 polyBasis = new WeightedStochasticBasis(sPoly);
	 RootEqns rEqns= new RootEqns(theGS,this);
	 NewtonSolver theNewt = new NewtonSolver();
	 double [][]theRes= theNewt.solveAtPts(rEqns,initWts);
	 return(theRes);
	}
	public GridPointsSpec getTheGS() {
		return theGS;
	}
	public void setTheGS(GridPointsSpec theGS) {
		this.theGS = theGS;
	}
	public static void main(String args[]){
		int [] someOrds={1,1,1,1};
		double [][] rngs={{-1,10},{-1,1},{-1,1},{-1,1}};
		double[][] initMat= new double[3][16];
		System.out.println("Hello World!!");
		Easy rcp = new Easy(rngs);
		double[][] theRes=rcp.tryIt(someOrds,initMat);

		System.out.println("largest root=("+theRes[0][0]+","+theRes[0][1]+")");

		System.out.println("2nd largest root=("+theRes[1][0]+","+theRes[1][1]+")");

		System.out.println("3rd largest root=("+theRes[2][0]+","+theRes[2][1]+")");

	}
	public GridVarSpec[] getVsArray() {
		return vsArray;
	}
	public void setVsArray(GridVarSpec[] vsArray) {
		this.vsArray = vsArray;
	}
	public GridVars getTheVars() {
		return theVars;
	}
	public void setTheVars(GridVars theVars) {
		this.theVars = theVars;
	}

}














