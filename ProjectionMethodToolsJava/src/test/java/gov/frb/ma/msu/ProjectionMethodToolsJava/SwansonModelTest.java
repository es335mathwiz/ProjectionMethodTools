package gov.frb.ma.msu.ProjectionMethodToolsJava;


import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.TestCase;


public class SwansonModelTest extends TestCase {
	public SwansonModelTest(String testName){
		super(testName);
		}
		//public static Test suite(){
		//return new TestSuite(SwansonModelTest.class);
		//}
		public void testApp(){
		GridVarSpec A=new GridVarSpec("A",-.003,.003);
		GridVarSpec K=new GridVarSpec("K",-.001+1.4319775094907936,.001+1.4319775094907936);
		GridVarSpec[] vsArray={A,K};
		GridVars theVars=new GridVars(vsArray);
		int [] theOrds={0,0};
		GridPointsSpec theGS = new GridPointsSpec(theVars,theOrds);

		double[][] stateWts={{0}, {1.4319775094907936}};
		StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
		sPoly.setTheWeights(stateWts);


		double [][] nonStateWts={{0.11148349932889678}, {0.4186970785697234}, {0.01010101020101014}, {-988.9135626421099}, 
				 {0.429593252917238}};
		String [] nsNames ={"C", "Inv", "r", "Welf", "Y"};
		NonStateVariablePolynomials nsPoly = new NonStateVariablePolynomials(nsNames);
		sPoly.setTheWeights(stateWts);
		sPoly.setTheWeights(nsPoly,nonStateWts);
		
		WeightedStochasticBasis theMod= new WeightedStochasticBasis(sPoly,nsPoly);
		SwansonModel hmModel = new SwansonModel();
		NewtonSolver aSolver=new NewtonSolver();

		double [][] initGuess={{0}, {1.4319775094907936}, {0.11148349932889678}, {0.4186970785697234}, {0.01010101020101014}, {-988.9135626421099}, 
				 {0.429593252917238}};

		double [][] theRes = aSolver.solveWSB(theMod,initGuess,hmModel);
		double [][] expRes={{0}, {1.4319775094907936}, {0.11148349932889678}, {0.4186970785697234}, {0.01010101020101014}, {-988.9135626421099}, {0.429593252917238}};
		MyAssert.assertArrayEquals(expRes, theRes, 1.0e-8);
		}
	

}
