package gov.frb.ma.msu.ProjectionTools;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class juddJetModelTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public juddJetModelTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( juddJetModelTest.class );
    }
    /*	ChebyshevPolyAtChebNodes capPoly8 = new ChebyshevPolyAtChebNodes(8);
	ChebyshevPolyAtChebNodes thetaPoly4 = new ChebyshevPolyAtChebNodes(4);
	ChebyshevPolyAtChebNodes[] thePolys = new ChebyshevPolyAtChebNodes[10];
	ChebyshevPolyAtChebNodes capPoly2 = new ChebyshevPolyAtChebNodes(2);
	ChebyshevPolyAtChebNodes thetaPoly3 = new ChebyshevPolyAtChebNodes(3);
    */
    public void testApp()
    {assertTrue(true);}
}

