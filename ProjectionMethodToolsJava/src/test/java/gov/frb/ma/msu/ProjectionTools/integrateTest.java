package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.GaussHermite;
import gov.frb.ma.msu.ProjectionTools.ScalarFunction;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class integrateTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public integrateTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( integrateTest.class );
    }

    /**
     * Rigourous Test :-)
     */
ScalarFunction[] hyperbolic = new ScalarFunction[2];
ScalarFunction sinh = hyperbolic[0] = new ScalarFunction() {
    public double evaluate(double d) {
        double e2d = Math.exp(d);
        return .5*(e2d-1/e2d);
    }
};
ScalarFunction cosh = hyperbolic[1] = new ScalarFunction() {
    public double evaluate(double dd) {
        double e2d = Math.exp(dd);
        return .5*(e2d+1/e2d);
    }
 
};


    public void testMeanStdDev()
    {
double yy;
	try{
	GaussHermite gh= new GaussHermite(80);
	yy=gh.integrate(cosh,3.,2.);
        assertEquals(74.39051927272446,yy,1e-8);
	} catch (Exception ee) {
	    System.err.println("Caught Exception:" + ee.getMessage());
//	    ee.printStackTrace();
	}
    }
    public void testStdDev()
    {
double yy;
	try{
	GaussHermite gh= new GaussHermite(80);
	yy=gh.integrate(cosh,0,2.);
        assertEquals(7.389056098905497,yy,1e-8);
	} catch (Exception ee) {
	    System.err.println("Caught Exception:" + ee.getMessage());
	    ee.printStackTrace();
	}
    }
    public void testMean()
    {
double yy;
	try{
	GaussHermite gh= new GaussHermite(80);
	yy=gh.integrate(cosh,3.,1.);
        assertEquals(16.598768435825846,yy,1e-7);
	} catch (Exception ee) {
	    System.err.println("Caught Exception:" + ee.getMessage());
	    ee.printStackTrace();
	}
    }
    public void testNoMeanNoStdDev()
    {
double yy;
	try{
	GaussHermite gh= new GaussHermite(80);
	yy=gh.integrate(cosh);
        assertEquals(1.648721270711612,yy,1e-7);
	} catch (Exception ee) {
	    System.err.println("Caught Exception:" + ee.getMessage());
	    ee.printStackTrace();
	}
    }


}
