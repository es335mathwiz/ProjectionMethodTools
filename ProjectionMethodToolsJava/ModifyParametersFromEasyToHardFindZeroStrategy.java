package gov.frb.ma.msu.ProjectionTools;

public class ModifyParametersFromEasyToHardFindZeroStrategy extends
		FindZeroAbstractClass {
double [] initialParameters;
double [] targetParameters;
public ModifyParametersFromEasyToHardFindZeroStrategy(
		double[] initialParameters, double[] targetParameters) {
	super();
	this.initialParameters = initialParameters;
	this.targetParameters = targetParameters;
}



public double [][] findCollocationWeights(WeightedStochasticBasis theCS,double [][] initWts, DoEqns theSys)  throws ProjectionRuntimeException{ 
	int numRows=initWts.length;int numCols=initWts[0].length;
	double [][] initNxt= new double[numRows][numCols];
	double fracNow=1;double [][] initNow=new double[numRows][numCols];
	double [][] initNowSafe=new double[numRows][numCols];
	boolean success=false;
	int ii,jj,kk,ll;int maxShrunkAttempts=20;
	for(kk=0;kk<numRows;kk++){
		for(ll=0;ll<numCols;ll++){
			initNow[kk][ll]=initWts[kk][ll];initNowSafe[kk][ll]=initNow[kk][ll];
		}
	} 	
	
	for(jj=0;jj<maxShrunkAttempts;jj++) {
		fracNow=1;
		for(ii=0;ii<theCS.getMaxShrink();ii++){
			for(kk=0;kk<numRows;kk++){
	    		for(ll=0;ll<numCols;ll++){
	    			initNxt[kk][ll]=initNowSafe[kk][ll];initNow[kk][ll]=initNowSafe[kk][ll];
	    		}
	    	}
			try {
			initNxt=solveProjParamsCloser(theCS,fracNow,initNow,theSys,
					adjustParams(fracNow,initialParameters,targetParameters),targetParameters);break;
			} catch(Exception ee) {fracNow=theCS.getShrinkFactor()*fracNow;


		
			
			}}
			
			for(kk=0;kk<numRows;kk++){
				for(ll=0;ll<numCols;ll++){
					initNow[kk][ll]=initNxt[kk][ll];initNowSafe[kk][ll]=initNow[kk][ll];
				}
				
			}
			initialParameters=adjustParams(fracNow,initialParameters,targetParameters);
			if(fracNow==1){success=true;break;}
			}
	if(!success) throw new ProjectionRuntimeException("trouble expanding getting params  " );
	return(initNow);
		
	}

double [] adjustParams(double frac,double[] paramsAway,double []targetParameters){
	int ii;int numParams=targetParameters.length;double [] paramsNow = new double [numParams];
	for(ii=0;ii<numParams;ii++){
		paramsNow[ii]=targetParameters[ii]*frac+paramsAway[ii]*(1-frac);
	}
	return(paramsNow);
}

	

    public double [][] solveProjParamsCloser(WeightedStochasticBasis theCS,double fracWidth, double [][] initWts, DoEqns theSys,
    		double [] paramsNow,double [] targetParameters)  throws ProjectionRuntimeException{
try { theSys.updateParams(adjustParams(fracWidth,paramsNow,targetParameters));}
catch (ProjectionRuntimeException ee) {	
   ee.printStackTrace();
   throw new ProjectionRuntimeException("trouble down below");

   }
FindZeroStrategyInterface fZero=new SimpleFindZeroStrategy();
	 double [][] theRes=fZero.findCollocationWeights(theCS,initWts,theSys,1.);
	 strategyItersSeq.addAll(fZero.getStrategyItersSeq());
    return(theRes);
    }
 
}
