package gov.frb.ma.msu.ProjectionTools;



import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.SSEqns;
import gov.frb.ma.msu.ProjectionTools.SSEvaluater;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class SSEqnsTest  extends TestCase {


	LSRoot3 eqSys= new LSRoot3();

//	FirstOne firstSys= new FirstOne();
	GridVars theVars;
	int [] theOrds=new int[5];
	WeightedStochasticBasis polyBasis;
	public SSEqnsTest(String name) {
		super(name);

			}
	  public static Test suite()
	    {
	        return new TestSuite( SSEqnsTest.class );
	    }
public void testZerothOrder(){
	theOrds[0]=0;theOrds[1]=0;theOrds[2]=0;theOrds[3]=0;theOrds[4]=0;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
    polyBasis=new WeightedStochasticBasis(sPoly,new NonStateVariablePolynomials(new String[0] ));
	double [] initSS={1,2};
	

	double [] initParams={3,4};

	NewtonSolver aSolver=new NewtonSolver();

	int ii;
	double [] expRes={3,1};
	SSEvaluater forBetter = new ForCheckingSS();
double [] initGuess={0,0};
	SSEqns better=new SSEqns(theGS,forBetter,initGuess);
	better.setParams(initParams);
	double [] betterResMat=aSolver.solveSS(better,initSS);
	for(ii=0;ii<2;ii++){
			assertEquals(expRes[ii],betterResMat[ii],1e-8);
		}

}


public void testFirstZerothOrder(){
	theOrds[0]=0;theOrds[1]=0;theOrds[2]=0;theOrds[3]=0;theOrds[4]=0;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
    polyBasis=new WeightedStochasticBasis(sPoly,new NonStateVariablePolynomials(new String[0] ));

  
	
	double [][] initWts={{1},{8},{2},{3},{4}};
	NewtonSolver aSolver=new NewtonSolver();//(1,2,1,2);
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	int ii;
	double [][] expRes=
	{{1.8767306707065323}, {1.016140256945672}, {1.0161402569456714}, {1.8767306707065323}, {1.8767306707065323}};
	
	for(ii=0;ii<2;ii++){
			assertEquals(expRes[ii][0],theResMat[ii][0],1e-8);
		}
}
public void testFirstOrder(){
	theOrds[0]=1;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
    polyBasis=new WeightedStochasticBasis(sPoly,new NonStateVariablePolynomials(new String[0] ));



	double [][] initWts={{1,0},{2,0},{2,0},{2,0},{2,0}};
	NewtonSolver aSolver=new NewtonSolver();
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	double [][] expRes=
	{{1.841972939908437, -0.12383561760482883}, {1.0099838883591183, 0.5614292969896807}, {1.0099838883591181, 0.5614292969896808}, {1.841972939908437, -0.12383561760482883}, {1.841972939908437, -0.12383561760482883}};	
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<2;jj++){
			assertEquals(expRes[ii][jj],theResMat[ii][jj],1e-8);
		}}
}

public void testSecondOrder(){
	theOrds[0]=2;//theOrds[1]=2;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
//	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(sPoly,nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly,new NonStateVariablePolynomials(new String[0] ));



   
	double [][] initWts={{1,0,0},{2,0,0},{2,0,0},{2,0,0},{2,0,0}};
	NewtonSolver aSolver=new NewtonSolver();//(3,8,3,8);
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	double [][] expRes=/*{{0.4917386475211406, -0.8517163215518192, 0.49173864752114055},{0.3341451274965266, -0.5787563379255645, 0.33414512749652653},{-0.009839332929656663, 0.01704222454675036, -0.009839332929656365},{0.49173864752114094, -0.8517163215518192, 0.49173864752114055},{0.49173864752114094, -0.8517163215518192, 0.49173864752114055}};*/
	                   /* {{1.7555332192090116, 0.36351664240890214, 0.06899474075788668}, {0.9995396594744403, -0.003917593069096766, -9.940242087760345E-4}, {0.43330432171936795, 0.4951609506601947, -0.06800071654911058}, {1.7555332192090116, 0.36351664240890214, 0.06899474075788668}, {1.7555332192090116, 0.36351664240890214, 0.06899474075788668}};*/
	{{1.8393677113207691, -0.13488259440912886, -0.0373629593857631}, {1.0092355859253122, 0.5752428302406359, -0.006904671020359187}, {1.0092355859253122, 0.5752428302406359, -0.006904671020359187}, {1.8393677113207691, -0.13488259440912886, -0.0373629593857631}, {1.8393677113207691, -0.13488259440912886, -0.0373629593857631}};
	
	
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<3;jj++){
			assertEquals(expRes[ii][jj],theResMat[ii][jj],1e-8);
		}}
}

	protected void setUp() throws Exception {
		super.setUp();
    	GridVarSpec rhoR=new GridVarSpec("rhoR",-.18,1.86);
      	GridVarSpec beta=new GridVarSpec("beta",-0.027,2.013);
      	GridVarSpec gamma2=new GridVarSpec("gamma2",-.72,1.32);
      	GridVarSpec tau=new GridVarSpec("tau",-.48,1.56);
      	GridVarSpec kappa=new GridVarSpec("kappa",-.44,1.6);
    	GridVarSpec[] vsArray={rhoR,beta,gamma2,tau,kappa};
    	theVars=new GridVars(vsArray);
 
  
			}
    	
	

	protected void tearDown() throws Exception {
		super.tearDown();
	}

}

