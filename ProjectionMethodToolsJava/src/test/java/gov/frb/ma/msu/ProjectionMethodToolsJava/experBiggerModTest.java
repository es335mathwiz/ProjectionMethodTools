package gov.frb.ma.msu.ProjectionMethodToolsJava;
//import Jama.Matrix; 
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.TestCase;

public class experBiggerModTest extends TestCase {
	final double tol = 1.0e-7;
	
	public 	experBiggerMod modEqns;
	public WeightedStochasticBasis thePM;
	double sml= 1e-8;double notSml=0.01;
	double [][] theRanges= {{-sml,sml}, {1.0, 1.5}, {-sml,sml}};
	String[] svbls={"aa","bigDelta","gg"};
	double [][] theShkRanges= { {-notSml,notSml}, {-sml,sml}};
	String[] shkvbls={"aa$Shock","gg$Shock"};
	String[] nsvbls={"bigPi", "CC", "FF", "HH", "RR", "SS", "varphi1", "varphi2", 
			"varphi3", "varphi4", "varphi5", "varphi6"};
	int  [] theOrders={0,0,0};
	int  [] theShkOrders={0,0};
	double [] easyParams={0.75, 11., 0.98, 1., 0.95, 0.95, 1., 1};
	public double [][] initArray={{0},  {1.1482719965510115}, 
			{1.00003967990983}, {0.6747671791773169}, 
			{2.0668809577284812}, {1.0127222107305078}, {0.9993505117630093}, 
			{0}, {0.8805516246685611}, {0.48341986166845025}, 
			{-0.0010637204556475056}, {0.0010367376656205925}, 
			{-0.8788729924864905}, {-0.0012079827424135007}, {0}};
	double [] pVec={1,2,3,4,5,6,7,8};
	double [] rVec;

	protected void setUp() throws Exception {
		super.setUp();
	modEqns= new experBiggerMod();
	thePM = new WeightedStochasticBasis(svbls,theRanges,theOrders,shkvbls,theShkRanges,theShkOrders,nsvbls);
	thePM.setTheShockVals(new double[2]);
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testUpdateParams() {
	

		modEqns.updateParams(pVec);
	assertEquals(1.,modEqns.get$alpha(),tol);
	assertEquals(2.,modEqns.get$anEps(),tol);	
	assertEquals(3.,modEqns.get$beta(),tol);
	assertEquals(4.,modEqns.get$chi(),tol);
	assertEquals(5.,modEqns.get$rho(),tol);
	assertEquals(6.,modEqns.get$rhoG(),tol);	
	assertEquals(7.,modEqns.get$sigma(),tol);
	assertEquals(8.,modEqns.get$tau(),tol);
	}

	public void testGetParams() {
		
		
		modEqns.updateParams(pVec);
		rVec=modEqns.getParams();
		assertEquals(1.,rVec[0],tol);
		assertEquals(2.,rVec[1],tol);	
		assertEquals(3.,rVec[2],tol);
		assertEquals(4.,rVec[3],tol);
		assertEquals(5.,rVec[4],tol);
		assertEquals(6.,rVec[5],tol);	
		assertEquals(7.,rVec[6],tol);
		assertEquals(8.,rVec[7],tol);
	
	}

	public void testUpdateValDrvCollocationSolution() {
		modEqns.updateParams(easyParams);
		thePM.setAllWeights(initArray);thePM.setShockVarLocs();
		EquationValDrv actRes=modEqns.updateValDrv(thePM);
		double [][] actValArray,actJacArray;
	double [][] expVal={{0.00038078915287301007}, {1.3758478826504161}, {-0.000010554493916757179}, 
			{-0.00017124507679290818}, {-2.9356092664611935}, {0.5043412782638976}, {0.}, 
			{1.1965727774674524}, {0.5770837857839457}, {0.002263236009305447}, 
			{0.005725398356277278}, {-0.0023786719175740156}, {1.00003967990983}, {0.}, 
			{0.}/*, {0.}, {-1.00003967990983}*/};
	double[][]expJac={{0., 0.00039006801369013467, 0., 0., -0.4832031563109424, 0., 
		-0.00044819527442927485, 0., 0., -2.0668809577284812, 0.42107320066343085, 0., 
		0., 0., 0.}, 
		{0., 2.0655385428591764, 0., 0., 0.6661784743613397, 0., 
		2.3776271506426125, 0., 0., -2.0668809577284812, 1., 4.131077085718353, 0., 0., 
		0.}, 
		{0.,  0., 0., 0.000023246868844448675, 0., 0., 0., 0., 0., 0., 1., 
		0., 0., -0.8718385824176308, 0.}, 
		{ 0., 0., 0., 0., 0., 0., 0., 0., 0., 
		0., 0., 1., 0., 1., 0.}, 
		{0., 0., 0.,  12.329006415005418, 0., 
		-0.001068852185845622, 0., 0., 0., 0., 0., 0., 3.3389657449938177, 
		0.8960809708514725, 0.}, 
		{0., 0.6374840500686006, 0., 
		-0.13904188461503694, 0.00018872301872615953, 0., -0.3654546371876563, 0., 0., 
		-0.7579285943357635, 0.3667016194143726, 0.7574363286292635, 
		-0.9902953295588096, 0., 0.}, 
		{1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 
		0., 0., 0., 0., 0.}, 
		{0., 0.7579285943357635, 0., 0., 1., 0., 
		-0.8708738025516896, 0., 0., 0., 0., 0., 0., 0., 0.}, 
		{0., 
		0.3667016194143726, 0., -0.21585555333158402, 0.2037239731146363, 
		0.9856177497355125, -0.421346860493014, 0., 0., 0., 0., 0., 0., 0., 0.}, 
		{0., 
		 0.7574363286292635, 0.,  -0.13930745220462712, 0., 0., 
		-1.7406163605220577, 0., 0.9902953295588096, 0., 0., 0., 0., 0., 0.}, 
		{0.,  
		1., 0., 0.07262042383210243, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}, 
		{ 0., 0., 0., -0.01948920260373507, 0., -0.8718385824176308, 0., 0., 1., 
		0., 0., 0., 0., 0., 0.}, 
		{0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 
		0., 0., 0., 0.}, 
		{0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 
		0., 0.}, 
		{0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 1.}/*, 
		{-1, 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}, {0., 0., 
		0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}*/};
	 actValArray=actRes.getTheVal().getArray();
	 actJacArray=actRes.getTheJac().getArray();
	
		MyAssert.assertArrayEquals(expVal,actValArray,tol);

			MyAssert.assertArrayEquals(expJac,actJacArray,tol);
		
	
	
	}

	public void testUseShock() {
	
		
		double[]someShocks={8,9};
		modEqns.setTheShocks(someShocks);
		thePM.setPerfectForesightQ(true);
		assertEquals(0.,modEqns.useShock(0, thePM));
		assertEquals(0.,modEqns.useShock(1, thePM));
		thePM.setPerfectForesightQ(false);
		assertEquals(8.,modEqns.useShock(0, thePM));
		assertEquals(9.,modEqns.useShock(1, thePM));
	}

	public void testDoShock() {
	
		modEqns.doShock(0, 99);modEqns.doShock(1, 88);

		assertEquals(99.,modEqns.getTheShocks()[0]);
		assertEquals(88.,modEqns.getTheShocks()[1]);

	}
/*
	public void testUpdateValDrvCollocationSolutionDoubleArray() {
		modEqns.updateParams(easyParams);
		thePM.setAllWeights(initArray);
		double[] tryVec={0, 0, 1.2, 0, 0};thePM.setEvalPt(new Matrix(tryVec,5));
		eqValDrv actRes=modEqns.updateValDrv(thePM);
		double [][] actValArray,actJacArray;
	double [][] expVal={{0.00038078915287301007}, {1.3758478826504161}, {-0.000010554493916757179}, 
			{-0.00017124507679290818}, {-3.261778323958574}, {0.5043412782638976}, {0.}, 
			{1.1965727774674524}, {0.5770837857839457}, {0.002263236009305447}, 
			{0.006220534603276651}, {-0.0023786719175740156}, {1.00003967990983}, {0.}, 
			{0.}, {0.}, {0.}};
	double [][] expJac=new double[17][1];
	 actValArray=actRes.getTheVal().getArray();
	 actJacArray=actRes.getTheJac().getArray();
	
		MyAssert.assertArrayEquals(expVal,actValArray,10*tol);
	
			MyAssert.assertArrayEquals(expJac,actJacArray,10*tol);
		
	

	}
*/
}
