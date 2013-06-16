package gov.frb.ma.msu.ProjectionMethodToolsJava;

//import ForCheckingRoot;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquateAtGridPoints;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
import gov.frb.ma.msu.ProjectionMethodToolsJava.RootEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class SpartanLSRootsTest extends TestCase {

	GridVars theVars;
	int [] theOrds=	{0,0,0,0,0};
	WeightedStochasticBasis polyBasis;
	
	
	public SpartanLSRootsTest(String name) {
		super(name);

			}
	  public static Test suite()
	    {
	        return new TestSuite( SpartanLSRootsTest.class );

	    }
public void testZerothOrder(){

	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	
  polyBasis=new WeightedStochasticBasis(sPoly);
		double [][] expRes={
		{1.686538478451125},{1.0005336836832166},{0.5013050382684785}};
	polyBasis.setAllWeights(1);
	   EquateAtGridPoints rEqns; 
	    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
 //    rEqns = new RootEqns(genCoeffs(polyBasis), polyBasis);

    double [][] newInit={{3},{4},{1}};
    NewtonSolver lilNewt=new NewtonSolver();//(5,7,5,7);
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
	
	int ii;
	for(ii=0;ii<2;ii++){
			assertEquals(expRes[ii][0],newRes[ii][0],1e-8);
		}
}
EquationValDrv[] genCoeffs(StochasticBasis polyB){
	StateVarTime VT;
	VT=new StateVarTime("rhoR");EquationValDrv rhoRAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("beta");EquationValDrv betaAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("gamma2");EquationValDrv gamma2AtPt=VT.evalVar(polyB);
	VT=new StateVarTime("tau");EquationValDrv tauAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("kappa");EquationValDrv kappaAtPt=VT.evalVar(polyB);
	
	EquationValDrv[] coeffs={rhoRAtPt.times(-1),
rhoRAtPt.plus(1).plus(rhoRAtPt.times(betaAtPt).plus(gamma2AtPt.times(tauAtPt).plus(kappaAtPt.times(tauAtPt))).plus(gamma2AtPt.times(rhoRAtPt).times(tauAtPt).times(-1))),
betaAtPt.times(gamma2AtPt.times(rhoRAtPt.times(tauAtPt))).plus(kappaAtPt.times(tauAtPt.times(-1))).plus(betaAtPt.times(gamma2AtPt.times(tauAtPt).times(-1))).plus(
		betaAtPt.times(rhoRAtPt).times(-1)).plus(betaAtPt.times(-1)).plus(-1),	betaAtPt};

return coeffs;}

public void testFirstOrder(){
	theOrds[0]=1;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);

    polyBasis=new WeightedStochasticBasis(sPoly);
	polyBasis.setAllWeights(2);
    EquateAtGridPoints rEqns;
 
    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
    double [][] newInit={{3,0},{4,0},{4,0}};
    NewtonSolver lilNewt=new NewtonSolver();
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
    ForCheckingRoot forRoot = new ForCheckingRoot();
 RootEqns newREqns=new RootEqns(theGS,forRoot);
 double [][] newNewRes=lilNewt.solveAtPts(newREqns, newInit);
  newRes=lilNewt.solveAtPts(rEqns, newInit);
	double [][] expRes=
		{{1.7571661240618452, 0.36033967018442575},{0.9996236465612487, -0.0036097410555406308},{0.43158742977972553, 0.4980300708711147}};
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<2;jj++){
			assertEquals(expRes[ii][jj],newRes[ii][jj],1e-8);
		}}
}
public void testSecondOrder(){
	theOrds[0]=2;//theOrds[1]=2;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);

    polyBasis=new WeightedStochasticBasis(sPoly);
	polyBasis.setAllWeights(3);
    EquateAtGridPoints rEqns;// = new RootEqns(genCoeffs(polyBasis), polyBasis);
    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
    double [][] newInit={{3,0,0},{4,0,0},{4,0,0}};
    NewtonSolver lilNewt=new NewtonSolver();//(5,7,15,23);
 //   NewtonSolver lilNewt=new NewtonSolver(0,0,0,0);
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
    


   



	double [][] expRes=
		{{1.7555332192090116, 0.36351664240890214, 0.06899474075788668},{0.9995396594744408, -0.003917593069096639, -9.940242087762565E-4},{0.4333043217193677, 0.4951609506601947, -0.06800071654911058}};
		 
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<3;jj++){
			assertEquals(expRes[ii][jj],newRes[ii][jj],1e-8);
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
    	
	
	
	
}
