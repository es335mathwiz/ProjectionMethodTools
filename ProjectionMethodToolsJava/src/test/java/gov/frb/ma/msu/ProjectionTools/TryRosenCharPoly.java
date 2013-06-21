package gov.frb.ma.msu.ProjectionTools;


		
import gov.frb.ma.msu.ProjectionTools.Basis;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.RootEqns;
import gov.frb.ma.msu.ProjectionTools.RootEvaluater;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
	public class TryRosenCharPoly extends RootEvaluater {	

	public 	double [] cmpCoeffs(){
			double[] theRes={
	0,-(1/getParams()[3]),-((getParams()[1] + 
	         getParams()[3]*(getParams()[1] + getParams()[5]*Math.pow(getParams()[3],3.)*Math.pow(-1 + getParams()[4],2.))*Math.pow(-1 + getParams()[4],2.))/
							(getParams()[3]*getParams()[1]*(-1 + getParams()[4]))),-1		};
			return(theRes);
		}
	public int	getNumRoots(){return 3;}
		public EquationValDrv[] genCoeffs(Basis polyBasis){
			return null;
		}

		public double[][] tryIt(int[] theOrds,double[][] initWts){

		GridVarSpec d0=new GridVarSpec("d0",900,1100);
		GridVarSpec d1=new GridVarSpec("d1",0,2);
		GridVarSpec a0=new GridVarSpec("a0",9,11);
		GridVarSpec bet=new GridVarSpec("bet",.8,1);
		GridVarSpec delta=new GridVarSpec("delta",0.00,0.04);
		GridVarSpec a1=new GridVarSpec("a1",0,2);
		GridVarSpec[] vsArray={d0,d1,a0,bet,delta,a1};
		GridVars theVars = new GridVars(vsArray);
		GridPointsSpec theGS;
		StateVariablePolynomials sPoly;
		Basis polyBasis;

		 theGS = new GridPointsSpec(theVars,theOrds);
		 sPoly= new StateVariablePolynomials(theGS);
		 polyBasis = new WeightedStochasticBasis(sPoly);
		 RootEqns rEqns= new RootEqns(theGS,this);
		 NewtonSolver theNewt = new NewtonSolver();
		 double [][]theRes= theNewt.solveAtPts(rEqns,initWts);
		 return(theRes);
		}
		public static void main(String args[]){
			int [] someOrds={0,0,0,0,0,0};
			double[][] initMat= {{1},{2},{3}};
			System.out.println("Hello World!!");
			TryRosenCharPoly rcp = new TryRosenCharPoly();
			double[][] theRes=rcp.tryIt(someOrds,initMat);



		}

	}