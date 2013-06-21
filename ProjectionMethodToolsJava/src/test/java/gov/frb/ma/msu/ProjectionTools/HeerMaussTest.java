package gov.frb.ma.msu.ProjectionTools;


import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NewtonSolver;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.TestCase;


public class HeerMaussTest extends TestCase {
public HeerMaussTest(String testName){
super(testName);
}
//public static Test suite(){
//return new TestSuite(heerMaussTest.class);
//}
public void testApp(){
GridVarSpec kk=new GridVarSpec("kk",0.1,60);
GridVarSpec zz=new GridVarSpec("zz",-.3,.3);
GridVarSpec[] vsArray={kk,zz};
GridVars theVars=new GridVars(vsArray);
int [] theOrds={0,0};
GridPointsSpec theGS = new GridPointsSpec(theVars,theOrds);

double[][] stateWts={{32.0226}, {0}};
StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
sPoly.setTheWeights(stateWts);


double [][] nonStateWts={{2.63978}, {0.37882}, {1.06047}};
String [] nsNames ={"cc", "lambda", "nn"};
NonStateVariablePolynomials nsPoly = new NonStateVariablePolynomials(nsNames);
sPoly.setTheWeights(stateWts);
sPoly.setTheWeights(nsPoly, nonStateWts);
WeightedStochasticBasis theMod= new WeightedStochasticBasis(sPoly,nsPoly);
HeerMaussModel hmModel = new HeerMaussModel();
NewtonSolver aSolver=new NewtonSolver();

double [][] initGuess={{32.0226}, {0}, {2.63978}, {0.37882}, {1.06047}};

double [][] theRes = aSolver.solveWSB(theMod,initGuess,hmModel);
double [][] expRes={{32.00612954145691}, {0.}, {2.557283162605786}, {0.3910399969086855}, {1.0599300488504058}};
MyAssert.assertArrayEquals(expRes, theRes, 1.0e-8);
}
}
