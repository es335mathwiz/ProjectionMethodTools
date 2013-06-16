package gov.frb.ma.msu.ProjectionMethodToolsJava;

public abstract class EquateAtGridPoints extends DoEqns {

	String[] theNames;
	private int numOriginalNS;
	private NonStateVariablePolynomials nState;
	private StateVariablePolynomials sState;
	public StateVariablePolynomials getsState() {
		return sState;
	}


	public void setsState(StateVariablePolynomials sState) {
		this.sState = sState;
	}
	private EquationValDrv[] valuesToEquate;
	private EquationValDrv[] theCoeffs;
	private GridPointsSpec theGrid;
	public GridPointsSpec getTheGrid() {
		return theGrid;
	}


	public void setTheGrid(GridPointsSpec theGrid) {
		this.theGrid = theGrid;
	}
	private WeightedStochasticBasis valuesToEquateModel;

	public EquateAtGridPoints() {
		super();
	}


	public EquateAtGridPoints(GridPointsSpec aGrid ,int numNames){
			setNumOriginalNS(0); 
		setTheNames(genNames(aGrid, numNames));
		NonStateVariablePolynomials augmentedNS=new NonStateVariablePolynomials(/*new StateVariablePolynomials(aGrid),*/getTheNames());
setsState(new StateVariablePolynomials(aGrid));
		setnState(augmentedNS);
		WeightedAtGridPointsBasis wagpb=new WeightedAtGridPointsBasis(getsState(),getnState());
		setValuesToEquateModel(wagpb);
	}	
	public String[] genNames(Basis theProjModel, int numNewNames) {
	int numNames,originalNumNames;String[]nsNames;
	if(theProjModel.getTheNonState()==null) {originalNumNames=0; nsNames=null;}else 
	{originalNumNames=theProjModel.getNonStateVarDim();
	nsNames=theProjModel.getTheNonState().getNonStateVariableNames();}
	GridVars noStateVars= new GridVars(new GridVarSpec[0]);
		theProjModel.getTheState().getTheGrid().setTheStateVars(noStateVars);
		if(theProjModel.getTheNonState()!=null){
		theProjModel.getTheState().getTheGrid().setTheStateVars(noStateVars);}
	numNames=numNewNames+originalNumNames;
	String[] theNames = new String[numNames];
	
	int ii;
	for(ii=0;ii<originalNumNames;ii++){
	theNames[ii]=nsNames[ii];
	}
	for(ii=originalNumNames;ii<numNames;ii++){
	theNames[ii]="aRoot"+ii;
	}
	return(theNames);
	}
	public String[] genNames(GridPointsSpec theGrid, int numNames) {
		String[] theNames = new String[numNames];
		int ii;
		for(ii=0;ii<numNames;ii++){
		theNames[ii]="aRoot"+ii;
		}
		return(theNames);
		}
	public int getNumOriginalNS() {
		return numOriginalNS;
	}

	public void setNumOriginalNS(int numOriginalNS) {
		this.numOriginalNS = numOriginalNS;
	}

	public String[] getTheNames() {
		return theNames;
	}

	public void setTheNames(String[] theNames) {
		this.theNames = theNames;
	}

	public NonStateVariablePolynomials getnState() {
		return nState;
	}

	public void setnState(NonStateVariablePolynomials nState) {
		this.nState = nState;
	}

	public EquationValDrv[] getValuesToEquate() {
		return valuesToEquate;
	}

	public void setValuesToEquate(EquationValDrv[] valuesToEquate) {
		this.valuesToEquate = valuesToEquate;
	}

	public EquationValDrv[] getTheCoeffs() {
		return theCoeffs;
	}

	public void setTheCoeffs(EquationValDrv[] theCoeffs) {
		this.theCoeffs = theCoeffs;
	}

	public double[] getParams() {
		return null;
	}

	public void updateParams(double[] paramVec) {
	}
	@Override
	public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException {
		int numRoots=getValuesToEquate().length;int ii;
		EquationValDrv[] lhs = new EquationValDrv[numRoots];NonStateVarTime NVT;
		for(ii=0;ii<numRoots;ii++){
			NVT=new NonStateVarTime(getTheNames()[ii+getNumOriginalNS()],0);
			lhs[ii]=NVT.evalVar(getValuesToEquateModel());
		}	
		EquationValDrv sys= getValuesToEquate()[0].times(-1).plus(lhs[0]);
		for(ii=1;ii<numRoots;ii++){
			sys=sys.augSys(getValuesToEquate()[ii].times(-1).plus(lhs[ii]));
		}
		return(sys);
	}
	public void setValuesToEquateModel(WeightedStochasticBasis valuesToEquateModel) {
		this.valuesToEquateModel = valuesToEquateModel;
	}
	public WeightedStochasticBasis getValuesToEquateModel() {
		return valuesToEquateModel;
	}
}