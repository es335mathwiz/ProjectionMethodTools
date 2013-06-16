package gov.frb.ma.msu.ProjectionTools;

import DhbIterations.IterativeProcess;

public class ParameterMorpher extends IterativeProcess {
private double [][] initWts;
private double [][] initNow;
private double [] paramsStart;
private double [] paramsTarget;
WeightedStochasticBasis theCS;
double [][] initNowSafe;
double [][] initNxt;
private DoEqns theSys;
double fracNow;
boolean success;
int numRows;
int numCols;
int maxShrunkAttempts=20;
double shrinkFactor=.1;int maxShrink=20;
	public ParameterMorpher(double[][] initWts, WeightedStochasticBasis basis, DoEqns theSys, 
			double[] paramsStart, double[] paramsTarget) {
	super();
	this.initWts = initWts;
	this.paramsStart = paramsStart;
	this.paramsTarget = paramsTarget;
	this.theCS = basis;
	this.theSys = theSys;
	numRows=initWts.length;
	numCols=initWts[0].length;

}

	
	@Override
	public double evaluateIteration() {
		int ii,jj,kk,ll;
    	for(jj=0;jj<maxShrunkAttempts;jj++) {
    		fracNow=1;
    		for(ii=0;ii<maxShrink;ii++){
    			for(kk=0;kk<numRows;kk++){
    	    		for(ll=0;ll<numCols;ll++){
    	    			initNxt[kk][ll]=initNowSafe[kk][ll];initNow[kk][ll]=initNowSafe[kk][ll];
    	    		}
    	    	}
    			try {
    			initNxt=solveProjParamsCloser(fracNow,initNow,theSys,
    					adjustParams(fracNow,paramsStart,paramsTarget),paramsTarget);break;
    			} catch(Exception ee) {fracNow=shrinkFactor*fracNow;
    		
    			
    			}}
    			
    			for(kk=0;kk<numRows;kk++){
    				for(ll=0;ll<numCols;ll++){
    					initNow[kk][ll]=initNxt[kk][ll];initNowSafe[kk][ll]=initNow[kk][ll];
    				}
    				
    			}
    			paramsStart=adjustParams(fracNow,paramsStart,paramsTarget);
    			if(fracNow==1){success=true;break;}
    			}
		return(1.-fracNow);
	}

	@Override
	public void finalizeIterations() 	 throws ProjectionRuntimeException{
    	if(!success) throw new ProjectionRuntimeException("trouble expanding getting params  " );
	super.finalizeIterations();
}

	@Override
	public void initializeIterations() 
	{ 
		    initNxt= new double[numRows][numCols];
		    fracNow=1;initNow=new double[numRows][numCols];
		    initNowSafe=new double[numRows][numCols];
		    success=false;
		    	int kk,ll;
		    	for(kk=0;kk<numRows;kk++){
		    		for(ll=0;ll<numCols;ll++){
		    			initNow[kk][ll]=initWts[kk][ll];initNowSafe[kk][ll]=initNow[kk][ll];
		    		}
		    	} 	
		    	
		super.initializeIterations();
	}

    double [] adjustParams(double frac,double[] paramsAway,double []paramsTarget){
    	int ii;int numParams=paramsTarget.length;double [] paramsNow = new double [numParams];
    	for(ii=0;ii<numParams;ii++){
    		paramsNow[ii]=paramsTarget[ii]*frac+paramsAway[ii]*(1-frac);
    	}
    	return(paramsNow);
    }

    public double [][] solveProjParamsCloser(double fracWidth, double [][] initWts, DoEqns theSys,
    		double [] paramsNow,double [] paramsTarget)  throws ProjectionRuntimeException{
try { theSys.updateParams(adjustParams(fracWidth,paramsNow,paramsTarget));}
catch (ProjectionRuntimeException ee) {
	   ee.printStackTrace();
   throw new ProjectionRuntimeException("trouble down below");

   }
FindZeroStrategyInterface fZero=new SimpleFindZeroStrategy();
return(fZero.findCollocationWeights(theCS,initWts,theSys,1.));
    }
  

}
