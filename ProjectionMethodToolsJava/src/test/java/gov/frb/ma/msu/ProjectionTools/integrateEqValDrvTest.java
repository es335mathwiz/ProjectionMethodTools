package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
import gov.frb.ma.msu.ProjectionTools.GridVarSpec;
import gov.frb.ma.msu.ProjectionTools.GridVars;
import gov.frb.ma.msu.ProjectionTools.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;
import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
//import Jama.Matrix; 
/**
 * Unit test for simple CollocationSolution.
 */
public class integrateEqValDrvTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public integrateEqValDrvTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( integrateEqValDrvTest.class );
    }

    /**
     * Rigourous Test :-)
     */
    public void testProjModel() throws Exception
    {
	/*create a two variable grid*/
	GridVarSpec kk=new GridVarSpec("kk",-1,25);
	GridVarSpec theta=new GridVarSpec("theta",-10,10);
	GridVarSpec[] vsArray={kk,theta};
	GridVars theVars=new GridVars(vsArray);
	int [] theOrds={1,2};
	GridPointsSpec theGS=new GridPointsSpec(theVars,theOrds);


	/*create a StateVariablePolynomials*/
	double [][] moreWts={{.1,0,0,0,0,.4},{0,0,0,.3,0,1}};
	StateVariablePolynomials moreSPoly=new StateVariablePolynomials(theGS);
	moreSPoly.setTheWeights(moreWts);
	/*create a NonStateVariablePolynomials*/
	double [][] nsWts={{.1,0,.2,0,0,.4}};
	String [] nsNames={"aName"};
	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(nsNames);
	moreSPoly.setTheWeights(moreWts);
//	moreSPoly.setTheWeights(moreWts);
	moreSPoly.setTheWeights(nSPoly,nsWts);
	/*create a CollocationSolution*/
	WeightedStochasticBasis pMod=new WeightedStochasticBasis(moreSPoly,nSPoly);

	int ii;int jj;


	for(ii=0;ii<3;ii++){
	for(jj=0;jj<6;jj++){
	    //	    System.out.println("gotEm"+gotEm[ii][jj]);
	    //	    assertEquals(theDrvs[ii][jj],cmpDrv1State[ii][jj],1e-6);
	}}
	for(ii=0;ii<6;ii++){
	for(jj=0;jj<6;jj++){
	    //	    System.out.println("gotDrvEm"+gotDrvEm[ii][jj]);
	    //	    assertEquals(theDrvs[ii][jj],cmpDrv1State[ii][jj],1e-6);
	}}


	/*chg wts */
	double [][] chgWts={{.1,0,0,0,0,.4}};
	pMod.setTheNonStateWeights(chgWts);


	/*re-evaluate derivs won't change*/
	//double [] chgRghtRes={0.2725644799999999};
	//	cmp1State=aNameFunc.evaluate(aState);
	//	assertEquals(chgRghtRes[0],cmp1State[0],1e-7);

	/*	cmpDrv1State=aNameFunc.evaluateDrv(aState);
	for(ii=0;ii<6;ii++){
	for(jj=0;jj<2;jj++){
	    assertEquals(theDrvs[ii][jj],cmpDrv1State[ii][jj],1e-6);
	}}
	*/

	//double [][] twoStates={{3.7,4.4},{3.7,4.4}};

	//	double [][] cmp2State=aNameFunc.evaluate(twoStates);
	//	assertEquals(chgRghtRes[0],cmp2State[0][0],1e-7);
	//	assertEquals(chgRghtRes[0],cmp2State[1][0],1e-7);

	//	double [][][] cmpDrv2States=aNameFunc.evaluateDrv(twoStates);






	//	double [][] cmpNodes=aNameFunc.evaluate();
	//	double [][][] cmpDrvNodes=aNameFunc.evaluateDrv();
	/*
double[][] rghtAtNodes={{0.24142135623730945, -0.04142135623730944, -0.18284271247461906, 
  0.3828427124746191, 0.24142135623730945, -0.04142135623730944}, 
 {0.537265121302012, -0.537265121302012, -0.7071067811865476, 
  0.7071067811865476, 0.16984165988453526, -0.16984165988453526}};

double [][][] rghtDrvAtNodes=
{{{1, 1}, {1, 1}, {1, 1}, {1, 1}, {1, 1}, {1, 1}}, 
 {{0.7071067811865476, 1}, {-0.7071067811865476, 1}, 
  {0.7071067811865476, 1}, {-0.7071067811865476, 1}, 
  {0.7071067811865476, 1}, {-0.7071067811865476, 1}}, 
 {{1, 0.8660254037844386}, {1, 0.8660254037844386}, {1, 0.}, {1, 0.}, 
  {1, -0.8660254037844386}, {1, -0.8660254037844386}}, 
 {{0.7071067811865476, 0.8660254037844386}, 
  {-0.7071067811865476, 0.8660254037844386}, 
  {0.7071067811865476, 0.}, {-0.7071067811865476, 0.}, 
  {0.7071067811865476, -0.8660254037844386}, 
  {-0.7071067811865476, -0.8660254037844386}}, 
 {{1, 0.4999999999999998}, {1, 0.4999999999999998}, {1, -1.}, 
  {1, -1.}, {1, 0.4999999999999998}, {1, 0.4999999999999998}}, 
 {{0.7071067811865476, 0.4999999999999998}, 
  {-0.7071067811865476, 0.4999999999999998}, 
  {0.7071067811865476, -1.}, {-0.7071067811865476, -1.}, 
  {0.7071067811865476, 0.4999999999999998}, 
  {-0.7071067811865476, 0.4999999999999998}}};
//	System.out.println("cmpNodes="+cmpNodes[0][1]);
//	System.out.println("cmpDrvNodes="+cmpDrvNodes[2][0][0]);
	int ll;
	for(ii=0;ii<6;ii++){
	for(jj=0;jj<6;jj++){
	for(ll=0;ll<2;ll++){
	    assertEquals(rghtDrvAtNodes[ii][jj][ll],cmpDrvNodes[ii][jj][ll],1e-6);
	}}}

	*/
	//simple state vars on [-1 1].
	GridVarSpec skk=new GridVarSpec("kk");
	GridVarSpec stheta=new GridVarSpec("theta");
	GridVarSpec[] svsArray={skk,stheta};
	GridVars stheVars=new GridVars(svsArray);
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);
	double [][] sWts={{1,0,0,0,0,0},{0,1,0,0,0,0}};
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);

	double [][] snsWts=
	    {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},
{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
	String [] snsNames={"aName1","aName2","aName3","aName4","aName5","aName6"};
	NonStateVariablePolynomials snSPoly=new NonStateVariablePolynomials(snsNames);
//	snSPoly.setTheWeights(snsWts);
	sSPoly.setTheWeights(sWts);
	sSPoly.setTheWeights(snSPoly,snsWts);
	WeightedStochasticBasis spMod=new WeightedStochasticBasis(sSPoly,snSPoly);
	double [][] xx =
 {{0, 0.001, 0, 0.002, 0, 0.0002}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};
	spMod.setTheStateWeights(xx);
double [][] rghtNxt={{1.6148568476360788e-6}, {-1.6082054058032375e-6}, 
{4.5674556211364525e-7}, {-4.5514364211373483e-7}, 
{-3.5863145279213177e-7}, {3.5758603495920094e-7}, {0.0030051790250791104}, 
{0.0029995285451185696}, {0.003004245726336016}, {0.003001716433663984}, 
		     {0.0030007686005469056}, {0.003000496269255414}};
double [][] rghtNxtJac=
{{1.000806395298105, 0.0025727699143037997, 0.003893965485761419, 
  0.0005002136543794994, -0.9995763785469419, -0.001717417638952736, 
  4.010244182629815e-6, 2.835670855751446e-6, 3.4729733375361822e-6, 
  2.455762997851911e-6, 2.005122091314907e-6, 1.4178354278757225e-6}, 
 {1.000803073832535, -0.002570421283475866, 0.002231926475548702, 
  -0.0004948571040219961, -0.9995937417625697, 0.0017186233995673428, 
  -4.007586121512694e-6, 2.8337913227107216e-6, -3.4706713890839435e-6, 
  2.4541352744913915e-6, -2.0037930607563462e-6, 1.4168956613553602e-6}, 
 {1.0008074197106185, 0.001136617377591364, 0.0037071067811865477, 
  2.0970562748477146e-6, -1.0007799344292443, -0.00113660182956829, 
  1.1330484949183545e-6, 8.0118627416998e-7, 0., 0., 
  -1.1330484949183545e-6, -8.0118627416998e-7}, 
 {1.0008045878893812, -0.0011346149775913641, 0.0022928932188134524, 
  -1.2970562748477144e-6, -1.0007940731707554, 0.00113460902956829, 
  -1.1324084949183544e-6, 8.007337258300206e-7, 0., 0., 
  1.1324084949183544e-6, -8.007337258300206e-7}, 
 {1.0008024378077816, 0.00012048180327646915, 0.0005236753637669671, 
  -0.0004919354236694446, -0.9995958110906032, 0.0007306306923909401, 
  -8.942905272318265e-7, -6.323588961565173e-7, 7.744783149465411e-7, 
  5.476388683806298e-7, -4.47145263615913e-7, -3.161794480782585e-7}, 
 {1.000800098685578, -0.00011882779410440289, -0.0006435634850770874, 
  0.0004899804333119413, -0.9995999457878854, -0.0007298050130055464, 
  8.938724661147053e-7, -6.320632823056506e-7, -7.741162633987797e-7, 
  5.473828592760687e-7, 4.4693623305735244e-7, -3.1603164115282515e-7}, 
 {0.0010006391213344221, 0.0007075587082161184, 0.0008665788990961487, 
  0.0006127638159840597, 0.0005003195606672108, 0.00035377935410805904, 
  1.0009876180857775, 0.00270091377637268, 0.004050909023630591, 
  0.0006111894942701986, -0.9994857671531056, -0.0016533457079182959}, 
 {0.0010003072888270918, -0.000707324067199967, 0.0008662915237149991, 
  -0.0006125606109033028, 0.0005001536444135457, -0.00035366203359998334, 
  1.000993453710992, -0.0027050401865341936, 0.00239680028666175, 
  -0.0006114404939001023, -0.9994985518233412, 0.001651313948038179}, 
 {0.0010007414213562373, 0.0007076310452552595, 0., 0., 
  -0.0010007414213562373, -0.0007076310452552595, 1.0009852847099603, 
  0.0012623869247615472, 0.0037071067811865477, 2.0970562748477146e-6, 
  -1.0009577994285859, -0.001262371376738473}, 
 {0.0010004585786437628, -0.0007074310452552595, 0., 0., 
  -0.0010004585786437628, 0.0007074310452552595, 1.0009907152900397, 
  -0.0012662269247615474, 0.0022928932188134524, -1.2970562748477144e-6, 
  -1.000980200571414, 0.001266220976738473}, {0.0010002437213780526, 
  0.0007072791182256888, -0.0008662364726892775, -0.0006125216839497038, 
  0.0005001218606890261, 0.00035363955911284423, 1.0009950361869564, 
  0.0002566694232365817, 0.00035688027467379257, -0.0006098773622358428, 
  -0.9994995119010157, 0.0007987245023709963}, 
 {0.0010000098684604336, -0.0007071137592418401, -0.0008660339501218704, 
  0.0006123784788689469, 0.0005000049342302166, -0.00035355687962091987, 
  1.0009998920162735, -0.00026010301307506786, -0.0008165895849661332, 
  0.0006123283618657464, -0.9995000491225376, -0.0008004426224908789}};

	/*	double [][] xx ={{.1,.2,.3,.4,.5,.6},{.7,.2,.4,.3,.2,.1}};*/

double [][] theVal=spMod.getTheState().computeNxtStateAtNodes().getArray();
double [][] theJac=spMod.getTheState().computeJacobianNxtStateAtNodes().getArray();

	for(ii=0;ii<12;ii++){
	    assertEquals(rghtNxt[ii][0],theVal[ii][0],1e-8);
	for(jj=0;jj<12;jj++){
	    assertEquals(rghtNxtJac[ii][jj],theJac[ii][jj],1e-8);
	}}
	for(ii=0;ii<12;ii++){
	for(jj=0;jj<12;jj++){
	}}

////	theVal=spMod.getTheState().computeCurStateAtNodes().getArray();
//theJac=spMod.getTheState().computeJacobianCurStateAtNodes().getArray();


	double [][]xxx= {{0, 0.001, 0, 0.002, 0, 0.0002}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};
spMod.setTheStateWeights(xxx);

}

    public void testNonStandCheb() throws Exception
    {

	int ii;int jj;

	int [] theOrds={1,2};
	GridVarSpec skk=new GridVarSpec("kk",-1,25);
	GridVarSpec stheta=new GridVarSpec("theta",-10,10);
	GridVarSpec[] svsArray={skk,stheta};
	GridVars stheVars=new GridVars(svsArray);
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);
	double [][] sWts={{1,0,0,0,0,0},{0,1,0,0,0,0}};
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);

	String [] snsNames={"aName1","aName2","aName3","aName4","aName5","aName6"};
	NonStateVariablePolynomials snSPoly=new NonStateVariablePolynomials(snsNames);
	sSPoly.setTheWeights(sWts);
	sSPoly.setTheWeights(snSPoly,sWts);
	WeightedStochasticBasis spMod=new WeightedStochasticBasis(sSPoly,snSPoly);
	double [][]xxx= 
   {{5., 0.001, 0, 0.002, 0, 0.0002}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};
	spMod.setTheStateWeights(xxx);
/*
double [] rghtTheVal=
{5.002002562330697, 4.997997437669303, 5.00056568542495, 4.99943431457505, 
 4.999553072587914, 5.000446927412086, 0.0031956066721101455, 
 0.0015364441354587322, 0.0037071067811865477, 0.0022928932188134524, 
 0.0012186068902629505, 0.00004934230216817293};
*/
//double [][] theVal=spMod.getTheState().computeCurStateAtNodes().getArray();
/*
double [][] rghtTheJac=
{{1, 0.7071067811865476, 0.8660254037844386, 0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {1, -0.7071067811865476, 0.8660254037844386, -0.6123724356957946, 
  0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {1, 0.7071067811865476, 0., 0., -1., -0.7071067811865476, 0, 0, 0, 0, 0, 0}, 
 {1, -0.7071067811865476, 0., 0., -1., 0.7071067811865476, 0, 0, 0, 0, 0, 0}, 
 {1, 0.7071067811865476, -0.8660254037844386, -0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {1, -0.7071067811865476, -0.8660254037844386, 0.6123724356957946, 
  0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 0.8660254037844386, 
  0.6123724356957946, 0.4999999999999998, 0.3535533905932736}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 0.8660254037844386, 
  -0.6123724356957946, 0.4999999999999998, -0.3535533905932736}, 
 {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 0., 0., -1., -0.7071067811865476}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 0., 0., -1., 0.7071067811865476}, 
 {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, -0.8660254037844386, 
  -0.6123724356957946, 0.4999999999999998, 0.3535533905932736}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, -0.8660254037844386, 
  0.6123724356957946, 0.4999999999999998, -0.3535533905932736}};
*/
//double [][] theJac=spMod.getTheState().computeJacobianCurStateAtNodes().getArray();

	for(ii=0;ii<12;ii++){
//	    	    	    assertEquals(rghtTheVal[ii],theVal[ii][0],1e-8);
	for(jj=0;jj<12;jj++){
//	    	    	    assertEquals(rghtTheJac[ii][jj],theJac[ii][jj],1e-8);
	}}

double [] rghtNxtTheVal=
{4.999519687595646, 4.99951969433807, 4.999519704873097, 4.999519712656567, 
 4.999519725388352, 4.999519734169408, 0.0024001120701575636, 
 0.0024000931153246295, 0.0024001205113454794, 0.0024001033234147128, 
 0.0024001258619287067, 0.002400110386469419};

double [][] theNxtVal=spMod.getTheState().computeNxtStateAtNodes().getArray();

double [][] rghtNxtTheJac=
    {
     {
      1.0000615876278602,
      -0.5382639461760338,
      3.7289711749677817E-4,
      -1.343073366510804E-4,
      -0.9999690019480298,
      0.5383291597771117,
      -1.076752607932528E-4,
      -7.613790707293904E-5,
      -9.324951120607151E-5,
      -6.593736171614411E-5,
      -5.3837630396626474E-5,
      -3.8068953536469566E-5,
     },
     {
      1.000061562099867,
      -0.538659112696026,
      2.0695875594098036E-4,
      -1.2045420822176489E-4,
      -0.9999691717368548,
      0.5385937907988345,
      -1.077297367655636E-4,
      7.617642740237174E-5,
      -9.329668878198851E-5,
      6.597072129999497E-5,
      -5.386486838278187E-5,
      3.8088213701185916E-5,
     },
     {
      1.000061595498179,
      -0.5383744696036279,
      3.7071067811877967E-4,
      -1.9959731086184406E-4,
      -1.0000613206453652,
      0.538374321617919,
      -1.0769957262448511E-4,
      -7.615509813366647E-5,
      -0.0,
      -0.0,
      1.0769957262448511E-4,
      7.615509813366647E-5,
     },
     {
      1.0000615737384364,
      -0.5385485919329875,
      2.292893218813763E-4,
      -1.2347345836900915E-4,
      -1.0000614685912501,
      0.5385485353106965,
      -1.0771088842166876E-4,
      7.616309961058956E-5,
      -0.0,
      0.0,
      1.0771088842166876E-4,
      -7.616309961058956E-5,
     },
     {
      1.0000615572097937,
      -0.5384523899727612,
      6.855058155887963E-5,
      -1.0331742203958407E-4,
      -0.999969191695048,
      0.5385176652601169,
      -1.0770443321733096E-4,
      -7.615853509182836E-5,
      9.327477526641316E-5,
      6.595522610453199E-5,
      -5.3852216608665505E-5,
      -3.8079267545914194E-5,
     },
     {
      1.0000615392206516,
      -0.5384706742300712,
      -4.836029819648218E-5,
      3.502819888157616E-5,
      -0.999969230340981,
      0.538405402003506,
      -1.0768564442385268E-4,
      7.614524940854956E-5,
      9.325850369395451E-5,
      -6.594372036530591E-5,
      -5.384282221192637E-5,
      3.80726247042748E-5,
     },
     {
      7.692799324103402E-5,
      5.4396305683808045E-5,
      6.662159640889307E-5,
      4.7108582594201624E-5,
      3.8463996620517064E-5,
      2.7198152841904056E-5,
      1.000089106025829,
      -0.5382444877302227,
      3.967287492092548E-4,
      -1.1745582826044781E-4,
      -0.9999552427490455,
      0.5383388890000173,
     },
     {
      7.692544068328533E-5,
      -5.439450075291457E-5,
      6.661938582903808E-5,
      -4.7107019478195805E-5,
      3.846272034164272E-5,
      -2.719725037645732E-5,
      1.0000891662306002,
      -0.5386786317640563,
      2.308646344054258E-4,
      -1.373582169941957E-4,
      -0.9999553696714882,
      0.5385840312648194,
     },
     {
      7.692878016427875E-5,
      5.439686212257067E-5,
      0.0,
      0.0,
      -7.692878016427875E-5,
      -5.439686212257067E-5,
      1.0000890833552447,
      -0.5383550327534964,
      3.7071067811877967E-4,
      -1.9959731086184406E-4,
      -1.000088808502431,
      0.5383548847677875,
     },
     {
      7.692660445110587E-5,
      -5.4395323661032203E-5,
      0.0,
      -0.0,
      -7.692660445110587E-5,
      5.4395323661032203E-5,
      1.0000891381832167,
      -0.5385680829388114,
      2.292893218813763E-4,
      -1.2347345836900915E-4,
      -1.0000890330360304,
      0.5385680263165203,
     },
     {
      7.69249517029081E-5,
      5.439415499157397E-5,
      -6.661896235960943E-5,
      -4.710672004009119E-5,
      3.846247585145407E-5,
      2.7197077495786996E-5,
      1.0000891813373745,
      -0.5384328567648246,
      4.462738531650061E-5,
      -1.202336763302268E-4,
      -0.9999553796312577,
      0.5385274318640854,
     },
     {
      7.692315283431103E-5,
      -5.4392882999390515E-5,
      -6.66174044937063E-5,
      4.71056184625469E-5,
      3.846157641715553E-5,
      -2.719644149969527E-5,
      1.0000892294831194,
      -0.538490254202435,
      -7.234076893102523E-5,
      5.19849523540171E-5,
      -0.999955385209747,
      0.538395612017324,
     }
};



double [][] theNxtJac=spMod.getTheState().computeJacobianNxtStateAtNodes().getArray();

	for(ii=0;ii<12;ii++){
	    	    	    assertEquals(rghtNxtTheVal[ii],theNxtVal[ii][0],1e-4);
	for(jj=0;jj<12;jj++){
	    	    	    assertEquals(rghtNxtTheJac[ii][jj],theNxtJac[ii][jj],1e-5);
	}}


    }
    public void testNonStandChebNonState() throws Exception
    {

	int ii;int jj;
	int [] theOrds={1,2};
	GridVarSpec skk=new GridVarSpec("kk",-1,25);
	GridVarSpec stheta=new GridVarSpec("theta",-10,10);
	GridVarSpec[] svsArray={skk,stheta};
	GridVars stheVars=new GridVars(svsArray);
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);
	double [][] sWts={{1,0,0,0,0,0},{0,1,0,0,0,0}};
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
	sSPoly.setTheWeights(sWts);

	double [][] snsWts=
	    {{1,0,0,0,0,0},{0,1,0,0,0,0}};
	String [] snsNames={"aName1","aName2"};
	NonStateVariablePolynomials snSPoly=new NonStateVariablePolynomials(snsNames);
	sSPoly.setTheWeights(sWts);
	sSPoly.setTheWeights(snSPoly,snsWts);
	WeightedStochasticBasis spMod=new WeightedStochasticBasis(sSPoly,snSPoly);

	double [][] snsWts03=
	    {{1,0,0,0,0,0},{0,1,0,0,0,0},{1,0,0,0,0,0}};
	String [] snsNames03={"aName1","aName2","aName3"};
	NonStateVariablePolynomials snSPoly03=new NonStateVariablePolynomials(snsNames03);
	sSPoly.setTheWeights(snsWts);
	sSPoly.setTheWeights(snSPoly03,snsWts03);
	WeightedStochasticBasis spMod03=new WeightedStochasticBasis(sSPoly,snSPoly03);


	/*
   {{5., 0.4, 0, 0.01, 0, 0.00}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};

*/
	double [][]xxxx= 
   {{5., 0.0004, 0, 0.0001, 0, 0.00}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0}};
	double [][]xxxx3= 
	    {{5., 0.0004, 0, 0.0001, 0, 0.00}, {0.002, 0.001, 0.001, 0.0002, -0.001, 0},
{5., 0.0004, 0, 0.0001, 0, 0.00}};
spMod.setTheStateWeights(xxxx);
//double [][] theVal=spMod.getTheState().computeCurStateAtNodes().getArray();
	spMod.setTheNonStateWeights(xxxx);
//double [][] theNVal=spMod.getTheNonState().computeCurStateAtNodes().getArray();
//	for(ii=0;ii<theNVal.length;ii++){
//	for(jj=0;jj<theNVal[0].length;jj++){
//	//       assertEquals(theNVal[ii][jj],theVal[ii][jj],1e-8);
//	}}


//double [][]  theJac=spMod.getTheState().computeJacobianCurStateAtNodes().getArray();
double [][] theNJac=spMod.getTheState().computeJacobianCurStateAtNodes(spMod.getTheNonState()).getArray();
// System.out.println("pre 2 dimsJacs("+theJac.length+","+theJac[0].length+")"+
//		    "pre 2 dimsNJacs("+theNJac.length+","+theNJac[0].length+")");

;/*
	for(ii=0;ii<theJac.length;ii++){
	for(jj=0;jj<theJac[0].length;jj++){
	    	    //	    	       System.out.println("pre 2 theJac="+theJac[ii][jj]);
	    //	    	       System.out.println("pre 2 theNJac="+theNJac[ii][jj]);
	       assertEquals(theNJac[12+ii][12+jj],theJac[ii][jj],1e-8);
	}}
*/

double [] [] rghtNJacNow=
{{1, 0.7071067811865476, 0.8660254037844386, 0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0}, {1, -0.7071067811865476, 0.8660254037844386, 
  -0.6123724356957946, 0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {1, 0.7071067811865476, 0, 0, 
  -1., -0.7071067811865476, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0}, {1, -0.7071067811865476, 0, 0, -1., 0.7071067811865476, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {1, 0.7071067811865476, 
  -0.8660254037844386, -0.6123724356957946, 0.4999999999999998, 
  0.3535533905932736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
 {1, -0.7071067811865476, -0.8660254037844386, 0.6123724356957946, 
  0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 
  0.8660254037844386, 0.6123724356957946, 0.4999999999999998, 
  0.3535533905932736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 0.8660254037844386, 
  -0.6123724356957946, 0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 0, 0, 
  -1., -0.7071067811865476, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 0, 0, -1., 0.7071067811865476, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 
  -0.8660254037844386, -0.6123724356957946, 0.4999999999999998, 
  0.3535533905932736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, -0.8660254037844386, 
  0.6123724356957946, 0.4999999999999998, -0.3535533905932736, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
  0.7071067811865476, 0.8660254037844386, 0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 
  0.8660254037844386, -0.6123724356957946, 0.4999999999999998, 
  -0.3535533905932736, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 1, 0.7071067811865476, 0, 0, -1., -0.7071067811865476, 0, 0, 0, 0, 0, 
  0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 0, 0, -1., 
  0.7071067811865476, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  1, 0.7071067811865476, -0.8660254037844386, -0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736, 0, 0, 0, 0, 0, 0}, 
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, 
  -0.8660254037844386, 0.6123724356957946, 0.4999999999999998, 
  -0.3535533905932736, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 0.8660254037844386, 
  0.6123724356957946, 0.4999999999999998, 0.3535533905932736}, 
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
  -0.7071067811865476, 0.8660254037844386, -0.6123724356957946, 
  0.4999999999999998, -0.3535533905932736}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 1, 0.7071067811865476, 0, 0, -1., 
  -0.7071067811865476}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 1, -0.7071067811865476, 0, 0, -1., 0.7071067811865476}, 
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
  0.7071067811865476, -0.8660254037844386, -0.6123724356957946, 
  0.4999999999999998, 0.3535533905932736}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 1, -0.7071067811865476, -0.8660254037844386, 
    0.6123724356957946, 0.4999999999999998, -0.3535533905932736}};


	for(ii=0;ii<theNJac.length;ii++){
	for(jj=0;jj<theNJac[0].length;jj++){
	       assertEquals(rghtNJacNow[ii][jj],theNJac[ii][jj],1e-8);
	}}


	;


//  theJac=spMod.getTheState().computeJacobianNxtStateAtNodes().getArray();
// theNJac=spMod.getTheState().computeJacobianNxtStateAtNodes(spMod.getTheNonState()).getArray();
// System.out.println("pre 2 nxt dimsJacs("+theJac.length+","+theJac[0].length+")"+
//		    "pre 2 nxt dimsNJacs("+theNJac.length+","+theNJac[0].length+")");
	for(ii=0;ii<theNJac.length;ii++){
	for(jj=0;jj<theNJac[0].length;jj++){
	    //	    	    	       System.out.println("pre 2 theJac="+theJac[ii][jj]);
	    //	    	    	       System.out.println("pre 2 theNJac="+theNJac[ii][jj];)
	    //	       assertEquals(theNJac[ii][jj],theJac[ii][jj],g1e-8);
	}}

	/*

System.out.println("fix tests vec={");
	for(ii=0;ii<12;ii++){
	    System.out.println(theNxtVal[ii][0]);
	}
System.out.println("};");
*/
/*
System.out.println("fix tests mat={");
	for(ii=0;ii<theNJac.length;ii++){
System.out.println("{");
	for(jj=0;jj<theNJac[0].length;jj++){
	    System.out.println(theNJac[ii][jj]);
	}
System.out.println("},");
}
System.out.println("};");
System.out.println("fix tests done");


	for(ii=0;ii<theNJac.length;ii++){
	   for(jj=0;jj<theNJac[0].length;jj++){
	       //	     System.out.println("to chk theNJac="+theNJac[ii][jj]+" rght theNJac="+rghtNJacNxt[ii][jj]);
	       assertEquals(rghtNJacNxt[ii][jj],theNJac[ii][jj],1e-8);
	}}

*/
	spMod03.setTheStateWeights(xxxx);
	spMod03.setTheNonStateWeights(xxxx3);
 //theJac=spMod03.getTheState().computeJacobianNxtStateAtNodes().getArray();
 //theNJac=spMod03.getTheState().computeJacobianNxtStateAtNodes(spMod03.getTheNonState()).getArray();


	for(ii=0;ii<theNJac.length;ii++){
	    //	    System.out.println("xxxxx");
	for(jj=0;jj<theNJac[0].length;jj++){
	    //	       System.out.println("theNJac="+theNJac[ii][jj]);
	}}/*
	for(ii=0;ii<theJac.length;ii++){
	    //	    System.out.println("xxxxx");
	for(jj=0;jj<theJac[0].length;jj++){
	    //	       System.out.println("theJac="+theJac[ii][jj]);
	}}

*/


	double[][] xxxx2=new double[2][6];
	for(ii=0;ii<2;ii++){
	for(jj=0;jj<6;jj++){
	    xxxx2[ii][jj]=2*xxxx[ii][jj];
	}}


	spMod.setTheNonStateWeights(xxxx2);
 //theNVal=spMod.getTheNonState().computeCurStateAtNodes().getArray();
//	for(ii=0;ii<theNVal.length;ii++){
//	for(jj=0;jj<theNVal[0].length;jj++){
	//       assertEquals(theNVal[ii][jj],2*theVal[ii][jj],1e-8);
//	}}

//	spMod.setTheStateWeights(xxxx);
//theVal=spMod.getTheState().computeNxtStateAtNodes().getArray();
//	spMod.setTheNonStateWeights(xxxx);


//theNVal=spMod.getTheNonState().computeNxtStateAtNodes(spMod.getTheNonState()).getArray();
//	for(ii=0;ii<theNVal.length;ii++){
//	for(jj=0;jj<theNVal[0].length;jj++){
	//       assertEquals(theNVal[ii][jj],theVal[ii][jj],1e-8);
//	}}



//	spMod.setTheNonStateWeights(xxxx2);
 //theNVal=spMod.getTheNonState().computeNxtStateAtNodes(spMod.getTheNonState()).getArray();
//	for(ii=0;ii<theNVal.length;ii++){
//	for(jj=0;jj<theNVal[0].length;jj++){
//	       assertEquals(theNVal[ii][jj],2*theVal[ii][jj],1e-8);
//	}}




    }
    public void testDrv() throws Exception
    {


	int ii;int jj;
	GridVarSpec skk=new GridVarSpec("xx",-1,20);
	GridVarSpec[] svsArray={skk};
	GridVars stheVars=new GridVars(svsArray);
	int [] theOrds={3};
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);
		double[][] initSWts=
{{0, 0.1, 0.1, 0.1}};
		StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
		sSPoly.setTheWeights(initSWts);
		double[][] initNSWts=
{{0, 0.1, 0.1, 0.1}, {0, 0.1, 0.1, 0.1}};
	String [] snsNames03={"yy","zz"};
	NonStateVariablePolynomials snSPoly=new NonStateVariablePolynomials(snsNames03);
sSPoly.setTheWeights(initSWts);
sSPoly.setTheWeights(snSPoly,initNSWts);
	StochasticBasis spMod03=new WeightedStochasticBasis(sSPoly,snSPoly);

spMod03.setPerfectForesightQ(false);
//		double[][] initWts=
		/*		{{0, 0.1, 0.1, 0.1},{0, 0.1, 0.1, 0.1},{0, 0.1, 0.1, 0.1}};*/
/*		    {{		    1,
		    1.062159894381468E-24,
		    4.999999999979769E-8,
				    -2.564280822424515E-24},{
		    2.0000010000000001,
		    0.0,
		    9.999999999959537E-8,
		    1.0390000333911476E-39},{
		    -1.1102230246206638,
		    3.999999999983814E-7,
		    1.5700924586774225E-23,0}};*/

//		eqns03IntMod theMod= new eqns03IntMod();



//eqns02IntMod theMod02= new eqns02IntMod();
/*		eqns02Mod theMod02= new eqns02Mod();*/
//NewtonSolver aSolver=new NewtonSolver();
//double [][]theRes02=aSolver.solve(spMod03,initWts,theMod02);
//double [][]	rootRes=someEqns.newtonsMethod(spMod,allWtsAgain13);
//		double [][]		theRes02=theMod02.newtonsMethod(spMod03,initWts);



/*double [][] expRes=
    {{    1.9500000000000002,
    1.05,
    -1.3738307951181275E-16,
	  1.0257123033311482E-16},
     {-9.11, -10.290000000000001, -1.1775692907723659E-16, -1.6411397263583303E-15},
     {-0.9110000000000004, -1.029, 7.850463863511343E-17, -3.077136986921869E-16}
   */  /*     {    0.3899999999999999,
    0.21000000000000005,
    -2.6985962803242636E-16,
    -9.61605308413084E-17},
     {    0.038999999999999924,
    0.02100000000000016,
    -7.911792459993752E-17,
    -4.247090112157788E-17}};*/



for(ii=0;ii<3;ii++){
for(jj=0;jj<3;jj++){
 //   assertEquals(expRes[ii][jj],theRes02[ii][jj],1e-8);/*wants 1.95*/
}}

/*

xtm1
[[19.20073509136851], [13.518176039833444], [5.481823960166558], [-0.20073509136851075]]

xt
[[1.0, 0.9238795325112867, 0.7071067811865475, 0.3826834323650896, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [1.0, 0.38268343236508984, -0.7071067811865475, -0.9238795325112868, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [1.0, -0.3826834323650897, -0.7071067811865476, 0.9238795325112867, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [1.0, -0.9238795325112867, 0.7071067811865475, -0.3826834323650896, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]
[[1.000000035355339], [0.9999999646446609], [0.9999999646446609], [1.000000035355339]]

xtp1
[[0.9999999845804989, -0.8095238204023958, 0.31065757456541443, 0.3065543782558267, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.9999999845804988, -0.8095238187917724, 0.31065761817835, 0.306554365646862, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.9999999845804988, -0.8095238069901971, 0.31065761817835, 0.30655433715533886, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.9999999845804989, -0.8095237919108729, 0.31065757456541443, 0.30655439005740187, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]
[[1.0000000155328792], [1.0000000155328803], [1.0000000155328803], [1.0000000155328792]]


yt
[[0.0, 0.0, 0.0, 0.0, 1.0, 0.9238795325112867, 0.7071067811865475, 0.3826834323650896, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 1.0, 0.38268343236508984, -0.7071067811865475, -0.9238795325112868, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 1.0, -0.3826834323650897, -0.7071067811865476, 0.9238795325112867, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 1.0, -0.9238795325112867, 0.7071067811865475, -0.3826834323650896, 0.0, 0.0, 0.0, 0.0]]
[[2.0000010707106783], [2.000000929289322], [2.000000929289322], [2.0000010707106783]]


ytp1
[[-3.083900213917557E-8, -2.8491522879456096E-8, -2.1806467537637487E-8, -1.1801575189334048E-8, 1.0, -0.8095238061566343, 0.3106575854686482, 0.3065543841566143, 0.0, 0.0, 0.0, 0.0], [-3.083900239572225E-8, -1.1801575287510219E-8, 2.1806467719043384E-8, 2.8491523116474327E-8, 1.0, -0.8095238128909847, 0.31065760727511615, 0.30655435140110043, 0.0, 0.0, 0.0, 0.0], [-3.083900239572225E-8, 1.1801575287510215E-8, 2.1806467719043387E-8, -2.8491523116474324E-8, 1.0, -0.8095238128909847, 0.31065760727511615, 0.30655435140110043, 0.0, 0.0, 0.0, 0.0], [-3.083900213917557E-8, 2.8491522879456096E-8, -2.1806467537637487E-8, 1.1801575189334048E-8, 1.0, -0.8095238061566343, 0.3106575854686482, 0.3065543841566143, 0.0, 0.0, 0.0, 0.0]]
[[2.0000010310657585], [2.0000010310657608], [2.0000010310657608], [2.0000010310657585]]

zt
[[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.9238795325112867, 0.7071067811865475, 0.3826834323650896], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.38268343236508984, -0.7071067811865475, -0.9238795325112868], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, -0.3826834323650897, -0.7071067811865476, 0.9238795325112867], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, -0.9238795325112867, 0.7071067811865475, -0.3826834323650896]]
[[-1.1102226550688508], [-1.1102228715472908], [-1.1102231776940366], [-1.1102233941724766]]

ztp1
[[3.8095238095083936E-8, 3.519541076219231E-8, 2.6937401187949944E-8, 1.4578416470992039E-8, 0.0, 0.0, 0.0, 0.0, 1.0, -0.8095238061566343, 0.3106575854686482, 0.3065543841566143], [3.8095238095083936E-8, 1.4578416470992047E-8, -2.6937401187949944E-8, -3.519541076219231E-8, 0.0, 0.0, 0.0, 0.0, 1.0, -0.8095238128909847, 0.31065760727511615, 0.30655435140110043], [3.8095238095083936E-8, -1.4578416470992043E-8, -2.6937401187949948E-8, 3.519541076219231E-8, 0.0, 0.0, 0.0, 0.0, 1.0, -0.8095238128909847, 0.31065760727511615, 0.30655435140110043], [3.8095238095083936E-8, -3.519541076219231E-8, 2.6937401187949944E-8, -1.4578416470992039E-8, 0.0, 0.0, 0.0, 0.0, 1.0, -0.8095238061566343, 0.3106575854686482, 0.3065543841566143]]
[[-1.1102233484301862], [-1.1102233484301889], [-1.1102233484301889], [-1.1102233484301862]]
ytp1drvx
[[0.0, 0.0, 0.0, 0.0, 0.0, 0.09523809523809523, -0.30839002139300353, 0.46323290598208466, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.09523809523809523, -0.30839002395847037, 0.46323291844292347, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.09523809523809523, -0.30839002395847037, 0.46323291844292347, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.09523809523809523, -0.30839002139300353, 0.46323290598208466, 0.0, 0.0, 0.0, 0.0]]
[[-3.083900213917557E-8], [-3.083900239572225E-8], [-3.083900239572225E-8], [-3.083900213917557E-8]]

xtm1 + int((eps+1)^2)
[[21.200735091368514], [15.518176039833445], [7.481823960166559], [1.7992649086314898]]
[[19.20073509136851], [13.518176039833444], [5.481823960166558], [-0.20073509136851075]]



 */

/*

double [][] expResLst=
    {{    0.10001453750000011,
    1.994999999999081E-5,
    5.512500000013708E-6,
	  1.1962443823839432E-16},
     {    0.200029075,
    3.990000000000937E-5,
    1.1024999999474547E-5,
	  5.128561644869782E-17},
     {    2.220446049250313E-16,
    3.9999999996639946E-7,
    6.133330675979352E-19,
	  8.013377570109029E-19}};*/
    /*
{{0.10149503750000001,
0.00199995,
5.050124999999996E-4,
7.666879123977522E-20},
{0.20299007500000002,
0.0039999,
0.0010100249999999995,
1.0390000333911476E-39},
{-1.1102230246206638E-23,
3.999999999983814E-7,
1.5700924586774225E-23,
 1.1368683772161603E-13}};
    */
//		double [][]		theRes=spMod03.root(theMod,initWts);
for(ii=0;ii<3;ii++){
for(jj=0;jj<3;jj++){
//	assertEquals(expResLst[ii][jj],theRes[ii][jj],1e-8);
}}



}
}
