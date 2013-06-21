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

/**
 * Unit test for simple App.
 */
public class juddModTest
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public juddModTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( juddModTest.class );
    }

    
    public void testApp()
    {	/*create a one variable grid*/
    	GridVarSpec kk=new GridVarSpec("kk",0.333,1.667);
 
    	GridVarSpec[] vsArray={kk};
    	GridVars theVars=new GridVars(vsArray);
    	int [] theOrds={2};
    	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);


    	double [][] moreWts={{0.068198,0.02,0}};
    	StateVariablePolynomials moreSPoly=new StateVariablePolynomials(theGS);
    	moreSPoly.setTheWeights(moreWts);
    	/*create a NonStateVariablePolynomials*/
    	double [][] nsWts={{0.064533,0.02,0}};
    	String [] nsNames={"cc"};
    	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(nsNames);
    	moreSPoly.setTheWeights(moreWts);
    	moreSPoly.setTheWeights(nSPoly,nsWts);
double [][] initWts={{1.9769198835631079, 2.16799953694906256, -0.023080116436891875}, 
		{3.1309984650983957, 0.5967108827455211, -.07952785069107807}};
    	/*create a CollocationSolution*/
    	WeightedStochasticBasis pMod=new WeightedStochasticBasis(moreSPoly,nSPoly);
juddMod fMod= new juddMod();
double [][] expRes={{0.9769198835631079, 0.16799953694906256, -0.023080116436891875}, 
		{3.1309984650983957, 0.5967108827455211, -0.07952785069107807}};

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
