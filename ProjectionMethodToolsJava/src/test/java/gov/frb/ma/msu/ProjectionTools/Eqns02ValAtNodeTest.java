package gov.frb.ma.msu.ProjectionTools;



import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class Eqns02ValAtNodeTest extends TestCase {


	Eqns02ValAtNode eqSys= new Eqns02ValAtNode();
	String [] nsNames=new String[1];
	GridVars theVars;
	int [] theOrds=new int[1];
	WeightedStochasticBasis polyBasis;
	public Eqns02ValAtNodeTest(String name) {
		super(name);

			}
	  public static Test suite()
	    {
	        return new TestSuite( Eqns02ValAtNodeTest.class );
		//  return new TestSuite( juddModTest.class );
	    }
public void testZerothOrder(){
	theOrds[0]=0;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly,nsPoly);


	double [][] expRes={
			{.125},{2.0}};
	double [][] initWts={{1},{8}};
	NewtonSolver aSolver=new NewtonSolver();//(1,2,1,2);
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	int ii;
	for(ii=0;ii<2;ii++){
			assertEquals(expRes[ii][0],theResMat[ii][0],1e-8);
		}
}
public void testFirstOrder(){
	theOrds[0]=1;
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);
	StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly,nsPoly);



	double [][] initWts={{1,0},{2,0}};
	NewtonSolver aSolver=new NewtonSolver();
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	double [][] expRes={{.1875,.25}, 
			{2,2}};
	
	
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
	NonStateVariablePolynomials nsPoly=new NonStateVariablePolynomials(nsNames);
    polyBasis=new WeightedStochasticBasis(sPoly,nsPoly);




	double [][] initWts={{1,0,0},{2,0,0}};
	NewtonSolver aSolver=new NewtonSolver();//(3,8,3,8);
	double [][] theResMat=aSolver.solveWSB(polyBasis,initWts,eqSys);
	double [][] expRes={ {0.1875, 0.25000000000000006, 0.06250000000000003}, {2.0, 2.0, 2.0354088784794528E-16}};
	int ii,jj;
	for(ii=0;ii<2;ii++){
for(jj=0;jj<3;jj++){
			assertEquals(expRes[ii][jj],theResMat[ii][jj],1e-8);
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
