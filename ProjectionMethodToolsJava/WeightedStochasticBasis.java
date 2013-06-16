package gov.frb.ma.msu.ProjectionTools;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

public class WeightedStochasticBasis extends StochasticBasis {
// TODO move all the static methods to a separate class
	StrategyIterSequenceInfo newtonItersSeq = new StrategyIterSequenceInfo();
	public StrategyIterSequenceInfo getNewtonItersSeq() {
		return newtonItersSeq;	}

	public void setNewtonItersSeq(StrategyIterSequenceInfo newtonIS) {
		this.newtonItersSeq = newtonIS;
	}


		  
		  public WeightedStochasticBasis(StateVariablePolynomials aState,NonStateVariablePolynomials aNState){
			  

			  //setcollocationsystem(aNState);

				setTheNonState(aNState);
				setTheState(aState);

				setShockVarLocs(initShockVarLocs());
			    }
		   public WeightedStochasticBasis(StateVariablePolynomials aState,int[] ghOrder,double theMean,double theStDev){
		     	super();

		    	setTheState(aState);
		    	setTheGaussHermite(new GaussHermite(ghOrder));
		   	 double[] gaussHermiteMean={theMean,0};
		   	 double [] gaussHermiteStDev={theStDev,0};
		   	 setGaussHermiteMean(gaussHermiteMean);
		   	 setGaussHermiteStDev(gaussHermiteStDev);
		   	 setTheShockVals(new double[2]);
		   	setPerfectForesightQ(false);

		  
		   			setShockVarLocs(initShockVarLocs());
		        }
		public WeightedStochasticBasis(StateVariablePolynomials aState,int[] ghOrder,double []theMean,double []theStDev){
		     	super();

		    	setTheState(aState);

		    	//setcollocationsystem(aState);

		      	 setGaussHermiteMean(theMean);
		      	 setGaussHermiteStDev(theStDev);
		      	setPerfectForesightQ(false);
		      	 setTheShockVals(new double[2]);
		    	setTheGaussHermite(new GaussHermite(ghOrder));
		   

		  
		   			setShockVarLocs(initShockVarLocs());
		        }	 
	
		   public WeightedStochasticBasis(StateVariablePolynomials aState,NonStateVariablePolynomials aNState,int[] ghOrder,double theMean,double theStDev){
		     	super();
		     	//setcollocationsystem(aNState);
		     	setTheNonState(aNState);
		    	setTheState(aState);
		     	 double[] gaussHermiteMean={theMean,0};
		       	 double [] gaussHermiteStDev={theStDev,0};
		       	 setGaussHermiteMean(gaussHermiteMean);
		       	 setGaussHermiteStDev(gaussHermiteStDev);
		       	setPerfectForesightQ(false);
		      	 setTheShockVals(new double[2]);
		      	 setShockVarLocs(initShockVarLocs());
		        }
	
		    public WeightedStochasticBasis(StateVariablePolynomials aState,NonStateVariablePolynomials aNState,int[] ghOrder,double[] theMean,double[] theStDev){
		    	//setcollocationsystem(aNState);
		     	setTheNonState(aNState);
		    	setTheState(aState);
		       	 setGaussHermiteMean(theMean);
		       	 setGaussHermiteStDev(theStDev);
		       	setPerfectForesightQ(false);
		      	 setTheShockVals(new double[2]);
			setTheGaussHermite(new GaussHermite(ghOrder));
		   			setShockVarLocs(initShockVarLocs());
		        }
		    public WeightedStochasticBasis(String[] stateVars, String[] nonStateVars,
		    		double[][] stateRanges,int [] statePowers)  throws ProjectionRuntimeException{
		
		
		     	GridVars stateVSpecs = new GridVars(stateVars,stateRanges);
		    	GridPointsSpec theGrid = new GridPointsSpec(stateVSpecs,statePowers);
		
		    setTheState(new StateVariablePolynomials(theGrid));
		 
		  if(nonStateVars.length>0) { 
			  setTheNonState(new NonStateVariablePolynomials(nonStateVars));
		  //setcollocationsystem(getTheNonState());
		  }else {//setcollocationsystem(getTheState());
			  }
		    
			setShockVarLocs(initShockVarLocs());

			

		    }

		    public WeightedStochasticBasis(String[] stateVars,double[][] stateRanges,
		    		int [] statePowers ,
		    		String[] shockVars,double[][] shockRanges,
		    		int [] shockPowers, String[] nonStateVars
		    		)  throws ProjectionRuntimeException{
		     	super();
		 
		     	int [] allPowers = new int[statePowers.length+shockPowers.length];
		    	System.arraycopy(statePowers, 0, allPowers, 0, statePowers.length);
		    	   System.arraycopy(shockPowers, 0, allPowers, statePowers.length, shockPowers.length);
		
		    	GridVars stateVSpecs = new GridVars(stateVars,stateRanges,shockVars,shockRanges);
		    	GridPointsSpec theGrid = new GridPointsSpec(stateVSpecs,allPowers);
		    	setPerfectForesightQ(true);
		    setTheState(new StateVariablePolynomials(theGrid));
		 
		    setTheNonState(new NonStateVariablePolynomials(nonStateVars));

		    //setcollocationsystem(getTheNonState());
			setShockVarLocs(initShockVarLocs());
		    }
		
		    
		    
		 
		    public WeightedStochasticBasis(String[] stateVars, String[] nonStateVars,
		    		double[][] stateRanges,int [] statePowers,int[] ghOrder,double [] theMean,double[] theStDev)  throws ProjectionRuntimeException{
		
		 
		     	GridVars stateVSpecs = new GridVars(stateVars,stateRanges);
		    	GridPointsSpec theGrid = new GridPointsSpec(stateVSpecs,statePowers);
		    	setPerfectForesightQ(false);
		    	
		    	
		     	 setGaussHermiteMean(theMean);
		     	 setGaussHermiteStDev(theStDev);
		       	 setTheShockVals(new double[2]);
		
		   	setTheGaussHermite(new GaussHermite(ghOrder));
		  
		
		    setTheState(new StateVariablePolynomials(theGrid));
				    if(nonStateVars.length>0) { setTheNonState(new NonStateVariablePolynomials(nonStateVars));
			  //setcollocationsystem(getTheNonState());
		    }else 
			    {//setcollocationsystem(getTheState());
		    	}
			    
		
		    
			setShockVarLocs(initShockVarLocs());
		    }
		    
		    public WeightedStochasticBasis(String[] stateVars,double[][] stateRanges,
		    		int [] statePowers ,
		    		String[] shockVars,double[] shockMeans,double[] shockStDevs,int [] intOrders,
		    		int [] shockPowers, String[] nonStateVars
		    		)  throws ProjectionRuntimeException{

		 
		this(mergeStateShockStringArray(stateVars,shockVars),nonStateVars,
				mergeStateShockDoubleArray(stateRanges,GaussHermite.computeRanges(shockMeans,shockStDevs,intOrders)),
				mergeStateShockPowerArray(statePowers,shockPowers),
				intOrders,shockMeans,shockStDevs
				);
			setNumberOfShocks(shockVars.length);

		setShockVarLocs();
		   }
			

	    
		    

	public void setTheNonStateWeights(double [][] theWts) throws ProjectionRuntimeException {

		getTheState().setTheWeights(getTheNonState(), theWts);
	}

	public void setTheStateWeights(double [][] theWts) throws ProjectionRuntimeException {
	if(getTheState()!=null)getTheState().setTheWeights(theWts);
	if(getTheNonState()!=null)getTheState().setTheWeights(theWts);
	}

	public void setAllWeights(int nodeDim){

		int stateVarDim=getTheState().getStateVariableNames().length;
		int nonStateVarDim;
		if(getTheNonState()==null) {nonStateVarDim=0;} else 
		    {nonStateVarDim=getTheNonState().getNonStateVarDim();};
	double [][] xx = new double[stateVarDim+nonStateVarDim][nodeDim];
	setAllWeights(xx);
	}
	public void setAllWeights(double [][]xxOriginal){
		double [][] xx =new double[xxOriginal.length][xxOriginal[0].length];
		multiArrayCopy(xxOriginal,xx);

	int stateVarDim;
	if(getTheNonState()==null)stateVarDim=getTheState().numberOfStateVars(); else
		stateVarDim=getTheState().numberOfStateVars();
	int nonStateVarDim;
	if(getTheNonState()==null) {nonStateVarDim=0;} else 
	    {nonStateVarDim=getTheNonState().getNonStateVarDim();};
	int nodeDim=xx[0].length;
	int ii;int jj;
	double [][] stateXX = new double[stateVarDim-getNumberOfShocks()][nodeDim];
	double [][] nonStateXX = new double[nonStateVarDim][nodeDim];

	for(ii=0;ii<stateVarDim-getNumberOfShocks();ii++){
	for(jj=0;jj<nodeDim;jj++){
	    stateXX[ii][jj]=xx[ii][jj];
	}}
	for(ii=0;ii<nonStateVarDim;ii++){
	for(jj=0;jj<nodeDim;jj++){
	    nonStateXX[ii][jj]=xx[stateVarDim-getNumberOfShocks()+ii][jj];
	}}

	{this.setTheStateWeights(stateXX);}
	if((getTheNonState() != null)&&(nonStateXX.length>0)){this.setTheNonStateWeights(nonStateXX);}

	}

	protected void updatePolyWeights(DoEqns eqns, double[][] stateXX, double[][] nonStateXX) {
		if(stateXX.length>0)setTheStateWeights(stateXX);
		if(getTheNonState() != null){setTheNonStateWeights(nonStateXX);}
		    EquationValDrv theEqValDrv=eqns.updateValDrv(this);
		    Utilities.chkNan(theEqValDrv.theVal.getArray(),"theEqValDrv.theVal");
	}


	public double [][] [] solveParamsRange(double [][] initWts, DoEqns theSys, double [] initParamVec,
			double[] termParamVec, int numVals) throws ProjectionRuntimeException {
			
				int numRows=initWts.length;int numCols=initWts[0].length;
			double [][][] theRes = new double[numVals][numRows][numCols];
			int ii;double [] paramsNow=new double[initParamVec.length];
			double[][]receive;
			theSys.updateParams(initParamVec);
			ParameterMorpher paramMorph = new ParameterMorpher(initWts, this, theSys, initParamVec,initParamVec);
			paramMorph.evaluate();
			
			FindZeroStrategyInterface fZero=new ModifyParametersFromEasyToHardFindZeroStrategy(initParamVec,termParamVec);
			receive=fZero.findCollocationWeights(this,initWts,theSys,1.);
			 newtonItersSeq.addAll(fZero.getStrategyItersSeq());
			final ProgressBarPanel pBarPanel= new ProgressBarPanel(0,numVals);
			JFrame frame = new JFrame("Progress for solveParamsRange");
			frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
			frame.setContentPane(pBarPanel);
			frame.pack();
			frame.setVisible(true);
			multiArrayCopy(receive,theRes[0]);
			for(ii=1;ii<numVals;ii++){
				final int prog=ii;
			try {
				SwingUtilities.invokeLater(new Runnable(){public void run(){pBarPanel.updateBar(prog);}});
				
			paramsNow=updateParams(initParamVec,termParamVec,ii,numVals);
			theSys.updateParams(paramsNow);
			fZero=new 
			ModifyParametersFromEasyToHardFindZeroStrategy(updateParams(initParamVec,termParamVec,ii-1,numVals),
					paramsNow);
			if(ii==1){
			
			receive=improveGuess(fZero.findCollocationWeights(this,initWts,theSys,1.),receive);
			 newtonItersSeq.addAll(fZero.getStrategyItersSeq());
			
			} else {
								receive=improveGuess(fZero.findCollocationWeights(this,initWts,theSys,1.),theRes[ii-1]);
								 newtonItersSeq.addAll(fZero.getStrategyItersSeq());
			}
			multiArrayCopy(receive,theRes[ii]);
			} catch (Exception ee){
				return(theRes);
			}}
			return(theRes);}


	public Basis incPowers(StateVariablePolynomials aState,int [] goalPowers) throws ProjectionRuntimeException {
	   if(getTheNonState()==null)
		  {if(isPerfectForesightQ()){
			   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers)));} else {
				   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers)/*,
						   getTheGaussHermite().theOrders,getGaussHermiteMean(),getGaussHermiteStDev()*/)
						   );}}
		 		else if(isPerfectForesightQ()){
		   return(new WeightedStochasticBasis(aState.incPowers(goalPowers),getTheNonState()));} else {
			   return(new WeightedStochasticBasis(aState.incPowers(goalPowers),getTheNonState(),
					   getTheGaussHermite().theOrders,getGaussHermiteMean(),getGaussHermiteStDev())
					   );  }
		   }


	  public WeightedStochasticBasis(StateVariablePolynomials aState){

	setTheState(aState);
	setTheNonState(null);
//setcollocationsystem(aState);

	setShockVarLocs(initShockVarLocs());

}
/*	  
	  private WeightedStochasticBasis(NonStateVariablePolynomials aNState){
		  

		  //setcollocationsystem(aNState);

			setTheNonState(aNState);
			setTheState(aNState.getTheSPoly());

			setShockVarLocs(initShockVarLocs());
		    }
		
	   private WeightedStochasticBasis(NonStateVariablePolynomials aNState,int[] ghOrder,double[] theMean,double[] theStDev){
	    	//setcollocationsystem(aNState);
	     	setTheNonState(aNState);
	    	setTheState(aNState.getTheSPoly());
	       	 setGaussHermiteMean(theMean);
	       	 setGaussHermiteStDev(theStDev);
	       	setPerfectForesightQ(false);
	      	 setTheShockVals(new double[2]);
		setTheGaussHermite(new GaussHermite(ghOrder));
	   			setShockVarLocs(initShockVarLocs());
	        }
*/	
	public WeightedStochasticBasis incPowers(int [] goalPowers) throws ProjectionRuntimeException {
	   if(getTheNonState()==null)
		  {if(isPerfectForesightQ()){
			   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers)));} else {
				   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers))
						   );}}
		 		else if(isPerfectForesightQ()){
		   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers),getTheNonState()));} else {
			   return(new WeightedStochasticBasis(getTheState().incPowers(goalPowers),getTheNonState(),
					   getTheGaussHermite().theOrders,getGaussHermiteMean(),getGaussHermiteStDev())
					   );  }
		   }   

	        
}
