package gov.frb.ma.msu.ProjectionMethodToolsJava;

public class ShrinkToMiddleThenExpandRangeFindZeroStrategy extends
		FindZeroAbstractClass {

	public ShrinkToMiddleThenExpandRangeFindZeroStrategy() {
		// TODO Auto-generated constructor stub
	}
	 
    public double [][] findCollocationWeights(WeightedStochasticBasis theCS,double [][] initWts, DoEqns theSys)  
    throws ProjectionRuntimeException{ 
	int numRows=initWts.length;int numCols=initWts[0].length;
	double [][] initNxt= new double[numRows][numCols];double shrinkFactor=.3;int maxShrink=7;
	double widthNow=1;double [][] initNow=new double [numRows][numCols];boolean success=false;
	int ii,jj,kk,ll;int maxShrunkAttempts=7;double [][]initRes=new double[numRows][numCols];
	boolean shrinkNotWorkedYetQ=true;double bestShrink=0;double lastShrinkTry=1;double windowGoal;
	int numGoals=10;int gg;
	if(initWts[0].length>1)numGoals=1;
	for(kk=0;kk<numRows;kk++){
		for(ll=0;ll<numCols;ll++){
			initNow[kk][ll]=initWts[kk][ll];
			initNxt[kk][ll]=initWts[kk][ll];
		}
	}
	for(gg=0;gg<numGoals;gg++){windowGoal=((gg+1.)/numGoals)*1.;
	for(jj=0;jj<maxShrunkAttempts;jj++) {
		widthNow=windowGoal;lastShrinkTry=windowGoal;
		for(ii=0;ii<maxShrink;ii++){
		
			try {
			initRes=solveProjWtsMiddle(theCS,widthNow,initNow,theSys);bestShrink=widthNow;
						for(kk=0;kk<numRows;kk++){
				for(ll=0;ll<numCols;ll++){
					initNow[kk][ll]=initRes[kk][ll];
					initNxt[kk][ll]=initNow[kk][ll];
				}
			}
						shrinkNotWorkedYetQ=false;
						widthNow=(lastShrinkTry+bestShrink)/2;
						lastShrinkTry=widthNow;
			;break;
					} catch(Exception ee) {
				System.out.println("catch block for solveProjWtsMiddle: "+ee.getMessage());
//				ee.printStackTrace();

				if(initWts[0].length>1)
						if(shrinkNotWorkedYetQ){
				widthNow=shrinkFactor*widthNow;} else{
					widthNow=bestShrink+.2*(lastShrinkTry-bestShrink);
					lastShrinkTry=widthNow;
				}
				
				Utilities.copyXxToState(initNxt, numRows, numCols, initNow);
				}}
		
			if(widthNow>=windowGoal){success=true;break;}
			}}
	if(!success||shrinkNotWorkedYetQ) throw new ProjectionRuntimeException("trouble expanding intervals " );
	return(initNow);
		
	}
		
    public double [][] solveProjWtsMiddle(StochasticBasis theCS,double fracWidth, double [][] initWts, DoEqns theSys)  throws ProjectionRuntimeException{

   
    	WeightedStochasticBasis newMod=theMiddle(theCS,fracWidth);
    	 FindZeroStrategyInterface fZero=new SimpleFindZeroStrategy();
    	 try{
    	 double [][] theRes=fZero.findCollocationWeights(newMod,initWts,theSys,fracWidth);
    	 strategyItersSeq.addAll(fZero.getStrategyItersSeq());
         return(theRes);
    	 } catch (Exception ee){
    	   	 strategyItersSeq.addAll(fZero.getStrategyItersSeq());	 
    	   	 throw new ProjectionRuntimeException("from solveProjWtsMiddle"+ee.getMessage());
    	 }

    
    }
    public WeightedStochasticBasis  theMiddle(StochasticBasis theCS,double frac) throws ProjectionRuntimeException{
    	if(theCS.getTheNonState()==null)
			  {if(theCS.isPerfectForesightQ()){
				   return(new WeightedStochasticBasis(theCS.getTheState().theMiddle(frac)));} else {
					   return(new WeightedStochasticBasis(theCS.getTheState().theMiddle(frac),
							   theCS.getTheGaussHermite().theOrders,theCS.getGaussHermiteMean(),theCS.getGaussHermiteStDev())
							   );}}
			 		else if(theCS.isPerfectForesightQ()){
				   return(new WeightedStochasticBasis(theCS.getTheState().theMiddle(frac),theCS.getTheNonState()));} else {
				   return(new WeightedStochasticBasis(theCS.getTheState().theMiddle(frac),theCS.getTheNonState(),
						   theCS.getTheGaussHermite().theOrders,theCS.getGaussHermiteMean(),theCS.getGaussHermiteStDev())
						   );  }
			   }
}
