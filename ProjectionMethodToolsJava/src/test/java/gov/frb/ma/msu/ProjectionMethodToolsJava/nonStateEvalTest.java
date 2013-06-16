package gov.frb.ma.msu.ProjectionMethodToolsJava;

//import DoMoreElab;
//import doEqns03;
//import doOneNS;
//import doOneNSLead;
//import doSingEqnDivideSclr;
//import doSingEqnDivideSelf;
//import doSingEqnExpSclr;
//import doSingEqnMinusSelf;
//import doSingEqnPlusSclr;
//import doSingEqnPlusSelf;
//import doSingEqnPowSclr;
//import doSingEqnTimesSclr;
//import doSingEqnTimesSelf;
//import doSingleEqn;
//import doSingleEqnLog;
//import doTwoNS;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVariablePolynomials;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
//import Jama.Matrix; 
/**
 * Unit test for simple CollocationSolution.
 */
public class nonStateEvalTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public nonStateEvalTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( nonStateEvalTest.class );
    }
    public void testNonStandChebNonState() throws Exception
    {
	/* counters for use in for loops*/
	int ii;int jj;

	/*linear and second degree polys*/
	int [] theOrds={1,2};

	/*two variables kk and theta non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",-1.0,25);
	GridVarSpec stheta=new GridVarSpec("theta",-10,10);
	GridVarSpec[] svsArray={skk,stheta};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= 
	    {{5., 0.0004, 0, 0.0001, 0, 0.00},
	     {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};


	/*create state variable polys*/
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);

	/*three non state variable*/
	double [][]sWts3= 
	    {{5., 0.0004, 0, 0.0001, 0, 0.00},
	     {0.002, 0.001, 0.001, 0.0002, -0.001, 0},
	     {5., 0.0004, 0, 0.0001, 0, 0.00}};
	String [] snsNames03={"aName1","aName2","aName3"};

	/*create CollocationSolution with two state three non state*/
	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(snsNames03);

	sSPoly.setTheWeights(sWts);
	sSPoly.setTheWeights(nSPoly,sWts3);
	WeightedStochasticBasis spMod=new WeightedStochasticBasis(sSPoly,nSPoly);


	DoEqns someEqns= new doEqns03();

	double[][] sWts2=new double[2][6];
	for(ii=0;ii<2;ii++){
	    for(jj=0;jj<6;jj++){
		sWts2[ii][jj]=2*sWts[ii][jj];
	    }}








	double [][] rghtRoot02= 
	    {
{5., 0,0,0,0,0}, 
{-3.5,0,0,0,0, 0},
{1.2214027581601699, 0,0,0,0,0}, 
{0., 0,0,0,0,0}, 
{0., 0,0,0,0,0}, 
};
 
	double [][] allWts=
	    {
{5., 0.0004, 0, 0.0001, 0, 0.00},
	     {0.002, 0.001, 0.001, 0.0002, -0.001, 0},
{2.7, 0.0004, 0, 0.0001, 0, 0.00},
	     {0.002, 0.001, 0.001, 0.0002, -0.001, 0},
	     {0., 0.0004, 0, 0.0001, 0, 0.00}};

	NewtonSolver aSolver=new NewtonSolver();
	
	double [][]	rootRes=aSolver.solveWSB(spMod,allWts,someEqns);
//	double [][] rootRes=someEqns.newtonsMethod(spMod,allWts);
	for(ii=0;ii<5;ii++){
	    for(jj=0;jj<6;jj++){

		assertEquals(rghtRoot02[ii][jj],rootRes[ii][jj],1e-6);
	    }}



    }
    public void testSingleEqn() throws Exception
    {

	/* counters for use in for loops*/
	int ii;int jj;

	/*linear and second degree polys*/
	int [] theOrds={1};

	/*two variables kk and theta non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",-1.0,25);
	GridVarSpec[] svsArray={skk};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= 
	    {{5.2, 0.0004}};


	/*create state variable polys*/
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);


	/*create CollocationSolution with one state no non state*/
	//	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(sSPoly,snsNames03);
	//	nSPoly.setTheWeights(sWts);
	WeightedStochasticBasis spMod=new WeightedStochasticBasis(sSPoly,new NonStateVariablePolynomials(new String[0] ));


	DoEqns someEqns= new doSingleEqn();


	double [][] rghtRoot02= 
	    {{5., 0}};
 
	double [][] allWts=
	    {{2., 1.0004}};

	NewtonSolver aSolver=new NewtonSolver();
	
	double [][]	rootRes=aSolver.solveWSB(spMod,allWts,someEqns);
	
	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRoot02[ii][jj],rootRes[ii][jj],1e-6);
	    }}

	someEqns= new doSingleEqnLog();

	double [][] allWtsAgain=
	    {{2., 1.0004}};
	double [][] rghtRootAgain= 
	    {{1.6487212707001282, 0}};


	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain[ii][jj],rootRes[ii][jj],1e-6);
	    }}

	someEqns= new doSingEqnTimesSclr();

	double [][] allWtsAgain01=
	    {{2., 1.0004}};
	double [][] rghtRootAgain01= 
	    {{0.15625, 0}};


	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain01,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain01[ii][jj],rootRes[ii][jj],1e-6);
	    }}


	someEqns= new doSingEqnTimesSelf();

	double [][] allWtsAgain02=
	    {{2., 1.0004}};
	double [][] rghtRootAgain02= 
	    {{0.7071067811865476, 0}};


	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain02,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain02[ii][jj],rootRes[ii][jj],1e-6);
	    }}



	someEqns= new doSingEqnDivideSelf();

	double [][] allWtsAgain03=
	    {{2., 1.0004}};
	double [][] rghtRootAgain03= 
	    {{.5, 0}};


	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain03,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain03[ii][jj],rootRes[ii][jj],1e-6);
	    }}




	someEqns= new doSingEqnDivideSclr();

	double [][] allWtsAgain04=
	    {{2., 1.0004}};
	double [][] rghtRootAgain04= 
	    {{1, 0}};



	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain04,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){
	
assertEquals(rghtRootAgain04[ii][jj],rootRes[ii][jj],1e-6);
	    }}



	someEqns= new doSingEqnPlusSclr();

	double [][] allWtsAgain05=
	    {{2., 1.0004}};
	double [][] rghtRootAgain05= 
	    {{3.7, 0}};


	rootRes=aSolver.solveWSB(spMod,allWtsAgain05,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain05[ii][jj],rootRes[ii][jj],1e-6);
	    }}



	someEqns= new doSingEqnPlusSelf();

	double [][] allWtsAgain06=
	    {{2., 1.0004}};
	double [][] rghtRootAgain06= 
	    {{.25, 0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain06,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){

assertEquals(rghtRootAgain06[ii][jj],rootRes[ii][jj],1e-6);
	    }}


	someEqns= new doSingEqnMinusSelf();

	double [][] allWtsAgain07=
	    {{2., 1.0004}};
	double [][] rghtRootAgain07= 
	    {{2, 0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain07,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){
	
assertEquals(rghtRootAgain07[ii][jj],rootRes[ii][jj],1e-6);
	    }}


	someEqns= new doSingEqnExpSclr();

	double [][] allWtsAgain08=
	    {{2., 1.0004}};
	double [][] rghtRootAgain08= 
	    {{0.4054651081081644, 0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain08,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){
	
assertEquals(rghtRootAgain08[ii][jj],rootRes[ii][jj],1e-6);
	    }}



	someEqns= new doSingEqnPowSclr();

	double [][] allWtsAgain09=
	    {{2., .0004}};
	double [][] rghtRootAgain09= 
	    {{1.224744871391589, 0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain09,someEqns);

	for(ii=0;ii<1;ii++){
	    for(jj=0;jj<2;jj++){
	
assertEquals(rghtRootAgain09[ii][jj],rootRes[ii][jj],1e-6);
	    }}


	
	double [][]nsWts= 
	    {{5., 0.0004}};
	String [] nsNames={"aName1"};

	/*create CollocationSolution with one state no non state*/
NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(nsNames);
sSPoly.setTheWeights(sWts);
sSPoly.setTheWeights(nSPoly,nsWts);
       spMod=new WeightedStochasticBasis(sSPoly,nSPoly);


	someEqns= new doOneNS();

	double [][] allWtsAgain10=
	    {{5., 0.0004},{0,0}};
	double [][] rghtRootAgain10= 
	    {{5, 0},{3,0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain10,someEqns);

	for(ii=0;ii<2;ii++){
	    for(jj=0;jj<2;jj++){
		assertEquals(rghtRootAgain10[ii][jj],rootRes[ii][jj],1e-6);
	    }}


	someEqns= new doOneNSLead();

	double [][] allWtsAgain11=
	    {{5.2, 0.0004},{0,0}};
	double [][] rghtRootAgain11= 
	    {{5, 0},{3,0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain11,someEqns);

	for(ii=0;ii<2;ii++){
	    for(jj=0;jj<2;jj++){
		assertEquals(rghtRootAgain11[ii][jj],rootRes[ii][jj],1e-4);
	    }}


	/*two non state variable*/
	double [][]nsWts02= 
	    {{0.1,0.2},{-.02,.1}};


	String [] nsNames02={"aName1","aName2"};

	/*create CollocationSolution with one state no non state*/
NonStateVariablePolynomials nSPoly02=new NonStateVariablePolynomials(nsNames02);
sSPoly.setTheWeights(sWts);
sSPoly.setTheWeights(nSPoly02,nsWts02);
        spMod=new WeightedStochasticBasis(sSPoly,nSPoly02);


	someEqns= new doTwoNS();

	double [][] allWtsAgain12=
	    {{5., 0.0004},{5.,0.0004},{5.,.0004}};
	
	double [][] rghtRootAgain12= 
		{{5.000107688994566, 2.0000000070724818E-4}, {3.0001076889945657, 2.0000000070724818E-4}, {2.000107688994566, 2.0000000070724818E-4}};
//newton epsilon 10^-9	    {{5, 0},{3,0},{2,0}};

	
	rootRes=aSolver.solveWSB(spMod,allWtsAgain12,someEqns);

	for(ii=0;ii<3;ii++){
	    for(jj=0;jj<2;jj++){
	
		assertEquals(rghtRootAgain12[ii][jj],rootRes[ii][jj],1e-4);
	    }}

    }
    public void testMoreElabEqn() throws Exception
    {



	/* counters for use in for loops*/
	int ii;int jj;

	/*linear and second degree polys*/
	int [] theOrds={1};

	/*two variables kk and theta non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",-1.0,25);
	GridVarSpec[] svsArray={skk};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= 
	    {{5.2, 0.0004}};


	/*create state variable polys*/
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);




	/*two non state variable*/
	double [][]nsWts02= 
	    {{0.1,0.2},{-.02,.1}};

	/*	    {{5., 0.0004},{5., 0.0004}};*/
	String [] nsNames02={"aName1","aName2"};

	/*create CollocationSolution with one state no non state*/
NonStateVariablePolynomials nSPoly02=new NonStateVariablePolynomials(nsNames02);
sSPoly.setTheWeights(sWts);
sSPoly.setTheWeights(nSPoly02,nsWts02);
	WeightedStochasticBasis      spMod=new WeightedStochasticBasis(sSPoly,nSPoly02);


	DoEqns someEqns= new DoMoreElab();

	double [][] allWtsAgain13=
	    {{5., 0.0004},{5.,0.0004},{5.,.0004}};
	NewtonSolver aSolver=new NewtonSolver();
	double [][]rootRes=aSolver.solveWSB(spMod,allWtsAgain13,someEqns);

	for(ii=0;ii<rootRes.length;ii++){
	    for(jj=0;jj<rootRes[0].length;jj++){

	    }}




    } 
   public void testMoreElabEqn02() throws Exception
    {



	/*linear and second degree polys*/
	int [] theOrds={2};

	/*two variables kk and theta non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",-1.0,25);
	GridVarSpec[] svsArray={skk};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= 
	    {{5.2, 0.0004,0.2}};


	/*create state variable polys*/
//	CollocationPolynomial sSPoly=new StateVariablePolynomials(stheGS);
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
		sSPoly.setTheWeights(sWts);





    }

}
