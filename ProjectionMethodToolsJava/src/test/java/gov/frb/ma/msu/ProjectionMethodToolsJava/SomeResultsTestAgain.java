package gov.frb.ma.msu.ProjectionMethodToolsJava;


//import MyAssert;
//import experBiggerMod;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionResults;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;
import junit.framework.TestCase;

public class SomeResultsTestAgain extends TestCase {
	final double tol = 1.0e-7;
	public ProjectionResults theRes;
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
			{0.}, {0.8805516246685611}, {0.48341986166845025}, 
			{-0.0010637204556475056}, {0.0010367376656205925}, 
			{-0.8788729924864905}, {-0.0012079827424135007}, {0.}};
	double [] pVec={1,2,3,4,5,6,7,8};
	double [] rVec;

	protected void setUp() throws Exception {
		super.setUp();

		modEqns= new experBiggerMod();
		modEqns.updateParams(easyParams);

	}
	
	

	protected void tearDown() throws Exception {
		super.tearDown();
	}

//	public void testToOrder() throws Exception{
//		do00000();	
//	int[] incArray={0, 1, 0, 0, 0};
//	ProjectionResults res00100 = gen00100(incArray);
//	incArray[3]=1;incArray[1]=0;
//	gen01100(incArray, res00100);
//	}



	public void do00000() throws Exception {
		thePM = new WeightedStochasticBasis(svbls,theRanges,theOrders,shkvbls,theShkRanges,theShkOrders,nsvbls);
//		thePM.setTheState(null);
		thePM.setTheShockVals(new double[2]);
		thePM.setAllWeights(initArray);
		theRes=new ProjectionResults(thePM,initArray,modEqns);
	double [][]expRes={{0.},  {1.1170701420763256}, {0.}, {0.8923511641122593}, 
			{0.9096955219974625}, {1.3077240089051505}, {1.0161937060039026}, {0.}, 
			{1.170133217666366}, {1.1166388761219266}, {0.015801385296395822}, 
			{-0.017659400326458744}, {-1.1516349603522378}, {0.017659400326458744}, {0.}};
	assertTrue(theRes.isConvergedQ());
	double [][] actArray=theRes.getResWeights();
	MyAssert.assertArrayEquals(expRes,actArray,tol);
	}



	private ProjectionResults gen00100(int[] incArray) throws Exception {
		ProjectionResults res00100=theRes.toOrder(incArray);
		theRes.collocateParamsRange(modEqns, easyParams, easyParams, 10);
		assertTrue(res00100.isConvergedQ());
		double[][] exp00100={{0., 0.}, {1.0941269673807148, 0.0638562252585503},  {0., 
			0.}, {0.8879191778355943, -0.12569463923778795}, {0.9155894578015996, 
				-0.06266258925444876}, {2.609745375924164, -0.6033102899422947}, 
				{0.9997704186228679, -0.010094742087467497}, {0., 0.}, {2.4041671608225643, 
				-0.6898667006469825}, {1.094039360676169, 0.06384594038639234}, 
				{-0.0003094709139006959, -0.010098669145066184}, {0.0006575444745056413, 
				0.011073217615028394}, {-2.407953231944587, 0.6931759305300121}, 
				{-0.0006575444745056413, -0.011073217615028394}, {0., 0.}};
		MyAssert.assertArrayEquals(exp00100,res00100.getResWeights(),tol);
		return res00100;
	}



	private void gen01100(int[] incArray, ProjectionResults res00100) throws Exception {
		ProjectionResults res01100=res00100.toOrder(incArray);
		assertTrue(res01100.isConvergedQ());
		double[][] exp01100={{0., 0., -0.01, 0.}, {1.0941269673807217, 0.06385622525855705, -3.4832674306423994e-16, 
			-6.907369310278665e-17}, {0., 0., 0., 0.}, {0.8879191778357544, -0.12569463923745453, 
				5.118679340425009e-17, 2.1845746541204775e-17}, {0.915612347633633, -0.06266415582608957, 
				0.00915597087733024, -0.0006266311144439659}, {2.6097453759317526, -0.6033102899472268, 
				1.5545167156346185e-15, -1.5282097383735878e-15}, {0.9997704186230875, -0.010094742087859153, 
				-1.9436131597563637e-16, -1.8796371179562346e-16}, {0., 0., 0., 0.}, {2.4041671608295565, 
				-0.6898667006516291, 1.3831020442237186e-15, -9.958010963112713e-16}, {1.094066711773902, 
				0.06384753654121429, -0.010940484776933901, -0.0006384647243689945}, {-0.00030947091388895063, 
				-0.010098669145738984, -2.244713658412823e-16, -9.042238264836932e-17}, 
				{0.0006575444745090371, 0.011073217615760043, 2.486406526568795e-16, 1.1319326570686747e-16}, 
				{-2.407953231969046, 0.6931759305494156, -1.8918675815380896e-15, 1.4110804573874078e-15}, 
				{-0.0006575444745090371, -0.011073217615760043, -2.486384948997061e-16, 
				-1.1319165073931124e-16}, {0., 0., 0., 0.}};
		MyAssert.assertArrayEquals(exp01100,res01100.getResWeights(),tol);
	}

	public void testToOrder()throws Exception {
		do00000();	
		int[] ordArray={0, 5, 0, 0, 0};
		ProjectionResults res00500=theRes.toOrder(ordArray);
		assertTrue(res00500.isConvergedQ());
		double [][] expRes={{0., 0., 0., 0., 0., 0.}, {1.0941942375118971, 0.0676453532126019, -0.022855900809799188, 
			0.004017762659211307, 0.00006900900665026674, -0.00023074392721946472}, {0., 0., 0., 0., 0., 
				0.}, {0.8876288375916535, -0.12277594950616175, -0.006394987375688727, 0.00388997282986263, 
				-0.00032388936864047004, -0.00014914475424676675}, {0.9159797348447672, -0.057909033578223364, 
				0.021165710151293128, -0.004678149854875531, 0.00046687834978835214, 0.00005379912883847725}, 
				{2.6575649119395077, -0.7203636506141936, 0.29649030718510855, -0.08222059871831013, 
				0.014359065555058477, -0.0007626210064199788}, {1.0000498403982205, 0.00007240993338421525, 
				-0.000013293464332575285, -0.000050164686811180915, 0.0000654626362945435, 
				-0.0000404357896818025}, {0., 0., 0., 0., 0., 0.}, {2.458372236834097, -0.831700795573861, 
				0.3529781908951045, -0.1043727653856709, 0.02137961547255206, -0.0025092889963958982}, 
				{1.0941942213587668, 0.06764535152552677, -0.022855897458177253, 0.0040177593236515674, 
				0.00006901607143003037, -0.0002307509988747745}, {0.000049825695859796175, 
				0.00007240914666734269, -0.000013290494643690575, -0.00005016805968646152, 
				0.00006546942255386072, -0.00004044242408253548}, {-0.000057026702883019406, 
				-0.00008198987772858661, 0.000015612232395692587, 0.00005328046205206247, 
				-0.0000689365640248568, 0.00004209291981907086}, {-2.4583649959586027, 0.8318032062501328, 
				-0.35311007591995924, 0.10451335238430143, -0.021496746490648633, 0.0025751615000903926}, 
				{0.00005702670288301941, 0.00008198987772858662, -0.000015612232395692583, 
				-0.00005328046205206247, 0.00006893656402485681, -0.00004209291981907087}, {0., 0., 0., 0., 0., 
				0.}};
		MyAssert.assertArrayEquals(expRes, res00500.getResWeights(), tol);
	}


}
