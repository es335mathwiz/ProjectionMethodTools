package gov.frb.ma.msu.ProjectionTools;
/**
 * Characterizes a variable for orthogonal polynomial.
 * Typically each variable will have a user provided name and upper
 * and lower values.
 */
public class GridVarSpec {

    private String varName;
    private double minVal;
    private double maxVal;
 
	public void setMaxVal(double maxVal) {
		this.maxVal = maxVal;
	}
	public void setMinVal(double minVal) {
		this.minVal = minVal;
	}
	public void setVarName(String varName) {
		this.varName = varName;
	}
	/**
	 * Default sets up for range (-1,1) as for Chebyshev polynomials
	 */
	public GridVarSpec() {
	super();
	varName="defaultVarName";
	minVal=-1;
	maxVal=1;
    }
	/**
	 * Creates variable with user supplied name and  a range (-1,1) as for Chebyshev polynomials
	 * @param aName
	 */
    public GridVarSpec(String aName) {
	varName=aName;
	minVal=-1;
	maxVal=1;
    }
    /**
     * Creates variable with user supplied names and upper and lower limits.
     * @param aName
     * @param theMin
     * @param theMax
     */
    public GridVarSpec(String aName, double theMin, double theMax) {
	varName=aName;
	minVal=theMin;
	maxVal=theMax;
    }
    public String getVarName(){
	return(varName);
    }
    public double getMinVal(){
	return(minVal);
    }
    public double getMaxVal(){
	return(maxVal);
    }
    /**
     * Creates a variable with lower range and upper range set so that the variable ranges over 
     * the middle fraction of the original variable range
     * @param frac should be between 0 and 1
     * @return GridVarSpec
     */
    public GridVarSpec  theMiddle(double frac){
  	  double newMin,newMax;
    double avg=(minVal+maxVal)/2;
    double width=(maxVal-minVal);
    newMin= avg - frac*width/2;
    newMax= avg + frac*width/2;
    return(new GridVarSpec(varName,newMin,newMax));
    }
    public void print(){
	System.out.println("varSpec: varName="+varName + 
" minVal=" + minVal +
			   "maxVal=" + maxVal);
    }
}
