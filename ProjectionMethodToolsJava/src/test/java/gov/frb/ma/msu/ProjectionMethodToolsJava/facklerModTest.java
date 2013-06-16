package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class facklerModTest
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public facklerModTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( facklerModTest.class );
    }

    
    public void testApp()
    {	/*create a one variable grid*/
    	GridVarSpec pp=new GridVarSpec("pp",0.01,5);

    	GridVarSpec[] vsArray={pp};
    	GridVars theVars=new GridVars(vsArray);
    	int [] theOrds={2};
    	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);


    	double [][] moreWts={{0.068198,0.02,0}};
    	StateVariablePolynomials moreSPoly=new StateVariablePolynomials(theGS);
    	moreSPoly.setTheWeights(moreWts);
    	/*create a NonStateVariablePolynomials*/
    	double [][] nsWts={{0.064533,0.02,0}};
    	String [] nsNames={"ss"};
    	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(nsNames);
    	moreSPoly.setTheWeights(moreWts);
       	moreSPoly.setTheWeights(nSPoly,nsWts);
double [][] initWts={{0.068198,0.02,0},{0.064533,0.02,0}};
    	/*create a WeightedStochasticBasis*/
    	WeightedStochasticBasis pMod=new WeightedStochasticBasis(moreSPoly,nSPoly);
facklerMod fMod= new facklerMod();
//when newton epsilon 10^-9
//double [][] expRes={{2.5050000000000003, 2.495000000000001, 3.700743415417188e-16}, 
//		{0.17641222439803947, 0.01676652332970139, -0.10894662516459787}};

//when newton epsilon 10^-7
double [][] expRes={{2.5050000000000003, 2.495, 2.5905203907920316E-16}, {0.17641221242537378, 0.016766544067053128, -0.10894663713741326}};
NewtonSolver aSolver=new NewtonSolver();
double [][] theRes=aSolver.solveWSB(pMod,initWts,fMod);

//double [][]		theRes=fMod.newtonsMethod(pMod,initWts);
		int ii,jj;
		for(ii=0;ii<2;ii++){
			for(jj=0;jj<3;jj++){
				assertEquals(expRes[ii][jj],theRes[ii][jj],1e-8);
			}}
			}
    	
    	
    
    }
