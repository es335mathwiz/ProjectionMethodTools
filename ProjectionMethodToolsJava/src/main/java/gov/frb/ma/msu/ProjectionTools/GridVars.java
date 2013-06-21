package gov.frb.ma.msu.ProjectionTools;
/**
 * Contains all the variables and shocks of the polynomials 
 */
public class GridVars {

	private int numberOfShocks;
    public GridVarSpec[] theVarSpecs;
    public GridVars() {
	super();
    }
    /**
     * Uses pre-specified vars with their pre-specified ranges
     * @param someVars
     */
    public GridVars(GridVarSpec[] someVars) {
	theVarSpecs=someVars;
    }
    /**
     * Uses user specified varNames and ranges to create polynomial variables
     * @param someVarNames
     * @param someRanges
     */
    public GridVars(String[] someVarNames, double[][] someRanges) {
    	int len=someVarNames.length;
    	int ii;
    	theVarSpecs=new GridVarSpec[len];
    	for(ii=0;ii<len;ii++){
    		theVarSpecs[ii]=new GridVarSpec(someVarNames[ii],
    				someRanges[ii][0],someRanges[ii][1]);
    	}
    	}
    /**
     * Uses varnames ranges and shock names and their ranges	
     * @param someVarNames
     * @param someRanges
     * @param someShockVarNames
     * @param someShockRanges
     */
    public GridVars(String[] someVarNames, double[][] someRanges,
    		String[] someShockVarNames, double[][] someShockRanges) {
    	int ii;
    	int len=someVarNames.length;int stochLen=someShockVarNames.length;
    	theVarSpecs=new GridVarSpec[len+stochLen];
    	for(ii=0;ii<len;ii++){
    		theVarSpecs[ii]=new GridVarSpec(someVarNames[ii],
    				someRanges[ii][0],someRanges[ii][1]);
    	}
    setNumberOfShocks(stochLen);
    	for(ii=0;ii<stochLen;ii++){
    		theVarSpecs[len+ii]=new StochasticVarSpec(someShockVarNames[ii],
    				someShockRanges[ii][0],someShockRanges[ii][1]);
    	}
    	}
    	   
    public String[] getVariableNames(){
	String [] varNames=null;
	if(theVarSpecs !=null) {
	    int ii;
	    varNames=new String[theVarSpecs.length];
	    for(ii=0;ii<theVarSpecs.length;ii++){
		varNames[ii]=theVarSpecs[ii].getVarName();
	    }
	}
	return(varNames);
    }
    public double[] getMinVals(){
	double [] minVals=null;
	if(theVarSpecs !=null) {
	    int ii;
	    minVals=new double[theVarSpecs.length];
	    for(ii=0;ii<theVarSpecs.length;ii++){
		minVals[ii]=theVarSpecs[ii].getMinVal();
	    }
	}
	return(minVals);
    }
    public double[] getMaxVals(){
	double [] maxVals=null;
	if(theVarSpecs !=null) {
	    int ii;
	    maxVals=new double[theVarSpecs.length];
	    for(ii=0;ii<theVarSpecs.length;ii++){
		maxVals[ii]=theVarSpecs[ii].getMaxVal();
	    }
	}
	return(maxVals);
    }
    public void print(){
	int ii;
    for(ii=0;ii<theVarSpecs.length;ii++){
	theVarSpecs[ii].print();
    }

    }
    /**
     * Creates a new collection of variables by taking the mid section of each 
     * of the variables
     * @param frac should be between 0 and 1
     * @return GirdVars
     */
    public GridVars  theMiddle(double frac){
    	int numVars=theVarSpecs.length;
    	GridVarSpec[] newVars = new GridVarSpec[numVars];
    	int ii;
    	for(ii=0;ii<numVars;ii++){
    		newVars[ii]=theVarSpecs[ii].theMiddle(frac);
    	} GridVars newvs= new GridVars(newVars);newvs.setNumberOfShocks(this.getNumberOfShocks());
      return(newvs);
      }
    public double [][] getRanges(){int ii;
    int numVars=theVarSpecs.length;
    double [] theMaxs = getMaxVals();
    double [] theMins = getMinVals();
    double[][]theRes=new double[numVars][2];
	for(ii=0;ii<numVars;ii++){
		theRes[ii][0]=theMins[ii];
		theRes[ii][1]=theMaxs[ii];
	}
		return(theRes);}
	public int getNumberOfShocks() {
		return numberOfShocks;
	}
	public void setNumberOfShocks(int numberOfShocks) {
		this.numberOfShocks = numberOfShocks;
	}
	public GridVarSpec[] getTheVarSpecs() {
		return theVarSpecs;
	}
	public void setTheVarSpecs(GridVarSpec[] theVarSpecs) {
		this.theVarSpecs = theVarSpecs;
	}

}
