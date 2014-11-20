package gov.frb.ma.msu.ProjectionMethodToolsJava;
//import gov.frb.ma.msu.ProjectionMethodToolsJava.Utilities;
public class ProjectionResults {
private	WeightedStochasticBasis theWeightedStochasticBasis;
private	double [][] resWeights;
private	double [][] initWeights;
private DoEqns theSystem; 
private	boolean convergedQ = false;

StrategyIterSequenceInfo theStrategyIters ;



public ProjectionResults(WeightedStochasticBasis thePM,double [][]initWts,DoEqns theSys) throws Exception{
	int ii;

	theStrategyIters = new StrategyIterSequenceInfo();

	FindZeroStrategyInterface fZero=new ShrinkToMiddleThenExpandRangeFindZeroStrategy();
	try{			initWeights=initWts;
		theSystem=theSys;
		theWeightedStochasticBasis=thePM;
	
	double [][] aRes = fZero.findCollocationWeights(thePM,initWts,theSys,1.);
	 theStrategyIters.addAll(fZero.getStrategyItersSeq());
	 setConvergedQ(true);
	setResWeights(new double[aRes.length][aRes[0].length]);
	
	for(ii=0;ii<aRes.length;ii++){System.arraycopy(aRes[ii], 0, getResWeights()[ii], 0, aRes[0].length);

	}
		} catch(Exception ee) {
			 theStrategyIters.addAll(fZero.getStrategyItersSeq());
			theWeightedStochasticBasis=thePM;
			initWeights=initWts;
			theSystem=theSys;
		}
	}

public ProjectionResults(WeightedStochasticBasis thePM,double [][]initWts,DoEqns theSys,
		FindZeroStrategyInterface fZero)
throws Exception{
	int ii;
	theStrategyIters = new StrategyIterSequenceInfo();
	try{			initWeights=initWts;
		theSystem=theSys;
		theWeightedStochasticBasis=thePM;
	double [][] aRes = fZero.findCollocationWeights(thePM,initWts,theSys,1.);
	 theStrategyIters.addAll(fZero.getStrategyItersSeq());
	setResWeights(new double[aRes.length][aRes[0].length]);
	for(ii=0;ii<aRes.length;ii++){System.arraycopy(aRes[ii], 0, getResWeights()[ii], 0, aRes[0].length);}

	setConvergedQ(true);
		} catch(Exception ee) {
			 theStrategyIters.addAll(fZero.getStrategyItersSeq());
			theWeightedStochasticBasis=thePM;
			initWeights=initWts;
			theSystem=theSys;
		}
	}
public ProjectionResults(WeightedStochasticBasis thePM,double [][]initWts,
		double[] easyParams, double [] hardParams,
		DoEqns theSys) throws Exception{
	int ii;
	theStrategyIters = new StrategyIterSequenceInfo();
	FindZeroStrategyInterface fZero=new ModifyParametersFromEasyToHardFindZeroStrategy(easyParams,hardParams);
	try{			initWeights=initWts;

		theWeightedStochasticBasis=thePM;

	double [][] aRes =fZero.findCollocationWeights(thePM,initWts,theSys,1.);
	 theStrategyIters.addAll(fZero.getStrategyItersSeq());	
		theSys.updateParams(hardParams);
		theSystem=theSys;
	setResWeights(new double[aRes.length][aRes[0].length]);
	for(ii=0;ii<aRes.length;ii++){System.arraycopy(aRes[ii], 0, getResWeights()[ii], 0, aRes[0].length);
	setConvergedQ(true);}
		} catch(Exception ee) {
			 theStrategyIters.addAll(fZero.getStrategyItersSeq());	
			theWeightedStochasticBasis=thePM;
			initWeights=initWts;
			theSystem=theSys;
		}
	}

public double [][]  collocateParamsTarget(
		DoEqns theSys,
		double [] initParamVec,double[] termParamVec)  throws Exception{


	FindZeroStrategyInterface fZero=new ModifyParametersFromEasyToHardFindZeroStrategy(initParamVec,termParamVec);
	try {
	 double [][] theRes=fZero.findCollocationWeights(theWeightedStochasticBasis,initWeights,theSys,1.);
	 theStrategyIters.addAll(fZero.getStrategyItersSeq());
	
	return(theRes);}
catch (Exception ee){
	   ee.printStackTrace();
		 theStrategyIters.addAll(fZero.getStrategyItersSeq());
			
	   throw new ProjectionRuntimeException("trouble down below");
}
}


private ProjectionResults incOrder(int [] theInc) throws Exception {
	int ii;int []oldOrd=getOrders();
	int numOrd=oldOrd.length;
	int[] newOrd=new int[numOrd];
	for(ii=0;ii<numOrd;ii++){
		newOrd[ii]=oldOrd[ii]+theInc[ii];
	}
	return(new ProjectionResults(theWeightedStochasticBasis.incPowers(newOrd),Utilities.nxtWts(getResWeights(),oldOrd,newOrd),theSystem));
	}


public ProjectionResults toOrder(int [] theGoal )throws Exception {
	int ii;int [] ordNow=getOrders();boolean goalNotAttained=false;
	int numPows=ordNow.length;int [] incVec= new  int[numPows];ProjectionResults nxtRes;
	for(ii=0;ii<numPows;ii++){
		if(ordNow[ii]<theGoal[ii]){goalNotAttained=true;incVec[ii]=1;break;}
	}
	if(goalNotAttained){
		nxtRes=incOrder(incVec);
		return(nxtRes.toOrder(theGoal));
	} else return(this);
	}


public double [][] [] collocateParamsRange(
		double [][]initWts, DoEqns theSys,
		double [] initParamVec,double[] termParamVec,
		int numVals)  throws Exception{
	try{
double[][][] theRes;
theRes=theWeightedStochasticBasis.solveParamsRange(
		initWts,theSys, initParamVec, termParamVec, numVals);
theStrategyIters.addAll(theWeightedStochasticBasis.getNewtonItersSeq());
return(theRes);}
	catch (Exception ee){
		theStrategyIters.addAll(theWeightedStochasticBasis.getNewtonItersSeq());
		   ee.printStackTrace();
		   throw new ProjectionRuntimeException("trouble down below");
		}
}

public double [][] [] collocateParamsRange(
		DoEqns theSys,
		double [] initParamVec,double[] termParamVec,
		int numVals)  throws Exception{

try {
	double[][][] theRes;
if(isConvergedQ()) theRes=theWeightedStochasticBasis.solveParamsRange(
		getResWeights(),theSys, initParamVec, termParamVec, numVals);
else theRes=theWeightedStochasticBasis.solveParamsRange(
		initWeights,theSys, initParamVec, termParamVec, numVals);
return(theRes);}
catch (Exception ee){
	   ee.printStackTrace();
	   throw new ProjectionRuntimeException("trouble down below");
}
}


public double[][] getRanges(){
return(theWeightedStochasticBasis.getRanges());
}
public double[] getParams(){
	return(theSystem.getParams());
}
public int [][] allPowers(int [] thePows) {

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
public int indexPower(int[]thePows,int sumSoFar,int[] theMaxPows){
	int ii;int newLen=theMaxPows.length-1;
	if(theMaxPows.length==1)return(1+sumSoFar+thePows[0]); else {
		int[] remainingPows= dropEnd(theMaxPows);
		int remProd=1; for(ii=0;ii<newLen;ii++){remProd=remProd*(remainingPows[ii]+1);};
return(indexPower(dropEnd(thePows),sumSoFar+thePows[newLen]*remProd,remainingPows));
	}
}
public int [] theIndices(int [] oldMaxPows,int [] newMaxPows){
	int [] [] allP=allPowers(oldMaxPows);int numAll=allP.length;int ii;
	int [] theRes =new int[numAll];
	for(ii=0;ii<numAll;ii++){
		theRes[ii]=indexPower(allP[ii],0,newMaxPows);
	}
	return(theRes);
}



public int [] dropEnd(int[] aList){
	int ii;int newLen=aList.length-1;
	int [] newList = new int[newLen];
	for(ii=0;ii<newLen;ii++){
		newList[ii]=aList[ii];
	}
	return(newList);
}


public int [] rawPows(int aPow) {
int [] theRes = new int[aPow+1];
int ii;
for(ii=0;ii<=aPow;ii++){
theRes[ii]=ii;}
return(theRes);
}
public int [] expand(int []aList,int blkSize,int totLen){
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
public int [] getOrders(){return(theWeightedStochasticBasis.getTheState().getTheGrid().getOrders());}
public boolean isConvergedQ() {
	return convergedQ;
}
public void setConvergedQ(boolean convergedQ) {
	this.convergedQ = convergedQ;
}
public double[][] getInitWeights() {
	return initWeights;
}
public void setInitWeights(double[][] initWeights) {
	this.initWeights = initWeights;
}
public double[][] getResWeights() {
	return resWeights;
}
private void setResWeights(double[][] resWeights) {
	this.resWeights = resWeights;
}
public Basis getTheWeightedStochasticBasis() {
	return theWeightedStochasticBasis;
}
public void setTheWeightedStochasticBasis(WeightedStochasticBasis theProjModel) {
	this.theWeightedStochasticBasis = theProjModel;
}
public DoEqns getTheSystem() {
	return theSystem;
}
public void setTheSystem(DoEqns theSystem) {
	this.theSystem = theSystem;
}


public StrategyIterSequenceInfo getTheStrategyIters() {
	return theStrategyIters;
}

public void setNewtonItersSeq(StrategyIterSequenceInfo newtonIS) {
	this.theStrategyIters = newtonIS;
}

}
