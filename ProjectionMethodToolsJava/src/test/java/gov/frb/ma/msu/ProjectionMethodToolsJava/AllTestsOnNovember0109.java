package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.SpartanLSRootsTest;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StatePolyTest;
import gov.frb.ma.msu.ProjectionMethodToolsJava.nonStateEvalTest;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTestsOnNovember0109 {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for gov.frb.ma.msu");
		//$JUnit-BEGIN$
		suite.addTestSuite(experBiggerModTest.class);
		suite.addTest(nonStateEvalTest.suite());
		suite.addTest(integrateTest.suite());
		suite.addTest(facklerModTest.suite());
		suite.addTest(integrateEqValDrvTest.suite());
		suite.addTest(juddModTest.suite());
		suite.addTest(equationsTest.suite());
		//		suite.addTestSuite(someResultsTest.class);
		suite.addTest(gridSpecTest.suite());
				suite.addTest(Eqns02RootsValAtNodeTest.suite());
				suite.addTest(SpartanLSRootsTest.suite());
				suite.addTest(AltEx1Test.suite());
		 
		suite.addTest(juddJetModelTest.suite());
		suite.addTest(StatePolyTest.suite());
		//$JUnit-END$
		return suite;
	}

}
