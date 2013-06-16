package gov.frb.ma.msu.ProjectionMethodToolsJava;

public abstract class StochasticBasis extends Basis {

	  public StochasticBasis(StateVariablePolynomials aState){

	setTheState(aState);
	setTheNonState(null);


	setShockVarLocs(initShockVarLocs());

}
public StochasticBasis(StateVariablePolynomials aState,NonStateVariablePolynomials aNState){


	setTheNonState(aNState);
	
	setTheState(aState);

	setShockVarLocs(initShockVarLocs());
}
	protected static double[][] mergeStateShockDoubleArray(double [][]stateArray, double [][] shockArray) {
		double[][] theRes = new double[stateArray.length+shockArray.length][2];
	
	   	System.arraycopy(stateArray,0,theRes,0,stateArray.length);
	   	System.arraycopy(shockArray,0,theRes,stateArray.length,shockArray.length);
		return(theRes);
	}
	protected static int[] mergeStateShockPowerArray(int []stateArray, int[] shockArray) {
		int[] theRes = new int[stateArray.length+shockArray.length];
		
		System.arraycopy(stateArray,0,theRes,0,stateArray.length);
	   	System.arraycopy(shockArray,0,theRes,stateArray.length,shockArray.length);
		return(theRes);
	}
	protected static String[] mergeStateShockStringArray(String []stateArray, String[] shockArray) {
		String[] theRes = new String[stateArray.length+shockArray.length];
		
		System.arraycopy(stateArray,0,theRes,0,stateArray.length);
	   	System.arraycopy(shockArray,0,theRes,stateArray.length,shockArray.length);
		return(theRes);
	}
	private double[] theShockVals=new double[0];// = { 0., 0. };
	

	private int [] shockVarLocs;
	private boolean perfectForesightQ = true;
	private GaussHermite theGaussHermite = new GaussHermite();
	private double[] gaussHermiteMean;
	private double[] gaussHermiteStDev;
	private boolean inNeedOfShockUpdatesQ = false;

	public boolean isInNeedOfShockUpdatesQ() {
		return inNeedOfShockUpdatesQ;
	}
	public void setInNeedOfShockUpdatesQ(boolean needsUpdateQ) {
		this.inNeedOfShockUpdatesQ = needsUpdateQ;
	}
	public StochasticBasis() {
//		super();

	}

    
	public double[] getGaussHermiteMean() {
		return gaussHermiteMean;
	}

	public void setGaussHermiteMean(double[] gaussHermiteMean) {
		this.gaussHermiteMean = gaussHermiteMean;
		setTheShockVals(new double[gaussHermiteMean.length]);
	}

	public double[] getGaussHermiteStDev() {
		return gaussHermiteStDev;
	}

	public void setGaussHermiteStDev(double[] gaussHermiteStDev) {
		this.gaussHermiteStDev = gaussHermiteStDev;
	}

	public boolean isPerfectForesightQ() {
		return perfectForesightQ;
	}

	public void setPerfectForesightQ(boolean perfectForesightQ) {
		this.perfectForesightQ = perfectForesightQ;
	}

	public GaussHermite getTheGaussHermite() {
		return theGaussHermite;
	}

	public void setTheGaussHermite(GaussHermite theGaussHermite) {
		this.theGaussHermite = theGaussHermite;
	}

	public double[] getTheShockVals() {

		return theShockVals;
	}
	public void setShockVarLocs() {setShockVarLocs(initShockVarLocs());
	}
	protected int[] initShockVarLocs() {
		int numShocks;int stateDim;int ii;int [] locVec;
		if(getTheNonState()!=null){
			stateDim=getTheState().numberOfStateVars();
			locVec = new int[stateDim];
			
		} else{
			System.out.println("stochbasis:line 120 nostatenull");
			locVec = new int[getTheState().getStateVariableNames().length];
			stateDim=getTheState().getTheGrid().getStateDim();
		
	}
		numShocks=getNumberOfShocks();
		for(ii=0;ii<stateDim-numShocks;ii++)locVec[ii]=-1;
	for(ii=0;ii<numShocks;ii++)locVec[stateDim-numShocks+ii]=ii;
	return(locVec);
	}
	public void setTheShockVals(double[] theShockVals) {
		this.theShockVals = theShockVals;
// TODO fix for not subclass
		if(getTheNonState()!=null)getTheState().setTheShockVals(theShockVals);
		if(getTheState()!=null)getTheState().setTheShockVals(theShockVals);
	}
	public int[] getShockVarLocs() {
		return shockVarLocs;
	}
	public void setShockVarLocs(int[] shockVarLocs) {

		int ii;int numShocks=0;
		for(ii=0;ii<shockVarLocs.length;ii++){if(shockVarLocs[ii]>=0)numShocks++;}
		this.shockVarLocs = shockVarLocs;
		setTheShockVals(new double[numShocks]);
	
	}
	public void updateForShocks(DoEqns theSys, int []shockLocs, double[] theShocks)
			throws ProjectionRuntimeException {
			
			getTheState().updateUsingNewNodePtVals();
			if(getTheNonState()!=null) getTheState().updateUsingNewNodePtVals(getTheNonState());
			}
	public int getNumberOfShocks() {
		if(getTheNonState()!=null){

			return(getTheState().getTheGrid().getNumberOfShocks());} else{
	return(getTheState().getTheGrid().getNumberOfShocks());}}
	public void setNumberOfShocks(int val) {
	if(getTheNonState()!=null){

		getTheState().getTheGrid().setNumberOfShocks(val);} else{
	getTheState().getTheGrid().setNumberOfShocks(val);}}
	public static int [] dropEnd(int[] aList) {
		int ii;int newLen=aList.length-1;
		int [] newList = new int[newLen];
		for(ii=0;ii<newLen;ii++){
			newList[ii]=aList[ii];
		}
		return(newList);
	}
	public static int [][] allPowers(int [] thePows) {
		int numPows=thePows.length;
		int ii,jj; int numAllPows=1;
		int[] blkSizes = new int[numPows+1];
		blkSizes[0]=1;
		for(ii=0;ii<numPows;ii++){
			numAllPows=numAllPows*(thePows[ii]+1);
			blkSizes[ii+1]=blkSizes[ii]*(thePows[ii]+1);
		}
		int [][] theRes= new int[numAllPows][numPows];
		int [] theInterMed= new int[numAllPows];
		for(ii=0;ii<numPows;ii++){
			theInterMed=expand(rawPows(thePows[ii]),blkSizes[ii],blkSizes[numPows]);
		for(jj=0;jj<numAllPows;jj++){theRes[jj][ii]=theInterMed[jj];};
			}
		
	return(theRes);
	}
	public static int [] rawPows(int aPow) {
	int [] theRes = new int[aPow+1];
	int ii;
	for(ii=0;ii<=aPow;ii++){
	theRes[ii]=ii;}
	return(theRes);
	}
	public static int [] expand(int []aList, int blkSize, int totLen) {
	int [] theRes = new int[totLen];int aLen=aList.length;
	int ii,jj,kk; int reps=totLen/(blkSize*aLen);
	
	
		
			for(ii=0;ii<aLen;ii++){
				for(kk=0;kk<reps;kk++){
				for(jj=0;jj<blkSize;jj++){
		theRes[kk*aLen*blkSize+jj+ii*blkSize]	=aList[ii];
		}
		}
	}
		return(theRes);}
	public static int indexPower(int[]thePows, int sumSoFar, int[] theMaxPows) {
		int ii;int newLen=theMaxPows.length-1;
		if(theMaxPows.length==1)return(1+sumSoFar+thePows[0]); else {
			int[] remainingPows= dropEnd(theMaxPows);
			int remProd=1; for(ii=0;ii<newLen;ii++){remProd=remProd*(remainingPows[ii]+1);};
	return(indexPower(dropEnd(thePows),sumSoFar+thePows[newLen]*remProd,remainingPows));
		}
	}
	protected int getNumStatePlusNonStatePolys(VarTimeType varTimeType) {
		  int stateVarDim =getStateVarDim();
		  int nonStateVarDim = getNonStateVarDim();
		  int numPolys = getNumPolys(varTimeType);
		  int stateWtDim=numPolys*stateVarDim;
		  int nonStateWtDim=numPolys*nonStateVarDim;
		  return(stateWtDim+nonStateWtDim);
	}
	int getNumStatePlusShockPolys(VarTimeType varTimeType) {
		  int stateVarDim =getStateVarDim();
		  int numShocks=getNumberOfShocks();
		  int numPolys = getNumPolys(varTimeType);
		  int stateWtDim=numPolys*stateVarDim;
		  int polyWtDim=numPolys*numShocks;
		  return(stateWtDim+polyWtDim);
	
	}
	protected int getNumPolys(VarTimeType varTimeType) {
		int numPolys=0;
		if(getTheState().getStateVariablePolynomialWeights()!=null)
		numPolys=getTheState().getStateVariablePolynomialWeights().getColumnDimension(); else
			if(getTheNonState().getRelevantWeightsNSP()!=null)
				numPolys=getTheNonState().getRelevantWeightsNSP().getColumnDimension();
		return numPolys;
	}
	protected int getNumEvalPts(VarTimeType varTimeType) {
		int numEvalPts;
		numEvalPts=getNumPolys(varTimeType);return(numEvalPts);
	}

}
