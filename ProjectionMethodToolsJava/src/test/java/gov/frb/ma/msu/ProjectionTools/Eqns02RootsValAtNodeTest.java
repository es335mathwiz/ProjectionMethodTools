package gov.frb.ma.msu.ProjectionTools;



import gov.frb.ma.msu.ProjectionTools.Basis;
import gov.frb.ma.msu.ProjectionTools.EquateAtGridPoints;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.RootEqns;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class Eqns02RootsValAtNodeTest extends TestCase {


	
	String [] nsNames=new String[1];
	GridVars theVars;
	int [] theOrds=new int[1];
	WeightedStochasticBasis polyBasis;
	public Eqns02RootsValAtNodeTest(String name) {
		super(name);

			}
	  public static Test suite()
	    {
	        return new TestSuite( Eqns02RootsValAtNodeTest.class );
		//  return new TestSuite( juddModTest.class );
	    }
public void testZerothOrder(){
	theOrds[0]=0;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	//NonStateVariablePolynomials nsPoly=new StateVariablePolynomials(sPoly);
  polyBasis=new WeightedStochasticBasis(sPoly);
		double [][] expRes={
		{-4.192582403567252},{1.1925824035672523},{0}};
	polyBasis.setAllWeights(1);
    EquateAtGridPoints rEqns;// = new RootEqns(genCoeffs(polyBasis), polyBasis);
    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
    double [][] newInit={{3},{4}};
    NewtonSolver lilNewt=new NewtonSolver();//(2,3,2,3);
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
	
	int ii;
	for(ii=0;ii<2;ii++){
			assertEquals(expRes[ii][0],newRes[ii][0],1e-8);
		}
}
EquationValDrv[] genCoeffs(Basis polyB){
	StateVarTime VT=new StateVarTime("xx"); EquationValDrv xxAtPoint=VT.evalVar(polyBasis);
    EquationValDrv[] coeffs={xxAtPoint.times(-5),xxAtPoint.times(3),xxAtPoint.pow(2)};
return coeffs;}

public void testFirstOrder(){
	theOrds[0]=1;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly);
	polyBasis.setAllWeights(2);
    EquateAtGridPoints rEqns;// = new RootEqns(genCoeffs(polyBasis), polyBasis);
    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
    double [][] newInit={{3,0},{4,0}};
    NewtonSolver lilNewt=new NewtonSolver();//(2,3,4,7);
 //   NewtonSolver lilNewt=new NewtonSolver(0,0,0,0);
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
 

	double [][] expRes={
			{-7.25199887706783, 6.292569072749704},{1.2519988770678294, -0.2925690727497027}};
	
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
	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly);
	polyBasis.setAllWeights(3);
    EquateAtGridPoints rEqns;// = new RootEqns(genCoeffs(polyBasis), polyBasis);
    rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
    double [][] newInit={{3,0,0},{4,0,0}};
    NewtonSolver lilNewt=new NewtonSolver();//(2,3,6,11);
 //   NewtonSolver lilNewt=new NewtonSolver(0,0,0,0);
    double [][] newRes=lilNewt.solveAtPts(rEqns, newInit);
    


   



	double [][] expRes={ {-10.256869140213945, 12.31086857734465, -6.064286736646691},{1.256869140213941, -0.31086857734464224, 0.06428673664668856}};
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<3;jj++){
			assertEquals(expRes[ii][jj],newRes[ii][jj],1e-8);
		}}
}

	protected void setUp() throws Exception {
		super.setUp();
    	GridVarSpec xx=new GridVarSpec("xx",0,2);
 
    	nsNames[0]="yy";
    	GridVarSpec[] vsArray={xx};
    	theVars=new GridVars(vsArray);
 
  
			}
    	
	

	protected void tearDown() throws Exception {
		super.tearDown();
	}

}
