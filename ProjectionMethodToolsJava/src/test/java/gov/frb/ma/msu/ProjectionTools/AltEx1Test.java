package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.Basis;
import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.SSEqns;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class AltEx1Test extends TestCase{


	AltEx1 eqSys= new AltEx1();

	GridVars theVars;
	int [] theOrds=new int[7];
	Basis polyBasis;
	
	public AltEx1Test(String name) {
		super(name);

			}
	  public static Test suite()
	    {
	        return new TestSuite( AltEx1Test.class );
	    }

	  
	  
	  
	  
	protected void setUp() throws Exception {
		super.setUp();
    	GridVarSpec alpha=new GridVarSpec("alpha",.324,.396);
      	GridVarSpec beta=new GridVarSpec("beta",.891,1.089);
      	GridVarSpec delta=new GridVarSpec("delta",0.0225,0.0275);
      	GridVarSpec psi=new GridVarSpec("psi",-.1,.1);
     	GridVarSpec rho=new GridVarSpec("rho",.855,1.045);
     	GridVarSpec tau=new GridVarSpec("tau",0.0225,0.0275);
     	GridVarSpec theta=new GridVarSpec("theta",2.655,3.245);
      	
    	GridVarSpec[] vsArray={alpha,beta,delta,psi,rho,tau,theta};
    	theVars=new GridVars(vsArray);
 
  
			}
    	
	

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testZerothOrder(){
		theOrds[0]=0;theOrds[1]=1;theOrds[2]=0;theOrds[3]=0;theOrds[4]=0;
		theOrds[5]=0;theOrds[6]=0;
		GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
		StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	    polyBasis=new WeightedStochasticBasis(sPoly,new NonStateVariablePolynomials(new String[0] ));
		//double [] initSS={0,0,.803,.291,5,1.08};
	    double [] initSS={ 0,0,
		   0.803592420141630,
		   0.291756310017320,
		   11.08360000000000000,
		   		   1.080682530956720};

		

		double [] initParams={0.36, 0.99, 0.025, 0.0, 0.95, 0.025, 2.95};

		NewtonSolver aSolver=new NewtonSolver();

		int ii;
		double [] expRes={3,1};

		SSEqns better=new SSEqns(theGS,eqSys,initSS);
		better.setParams(initParams);
double[][] initAtPts={{1,0},{2,0},{3,0},{4,0},{5,0},{6,0}};
		double [][] betterResMat=aSolver.solveAtPts(better,initAtPts);
//		betterResMat=aSolver.solveSS(better,initSS);
		for(ii=0;ii<2;ii++){
//				assertEquals(expRes[ii],betterResMat[ii],1e-8);
			}

	}

}
