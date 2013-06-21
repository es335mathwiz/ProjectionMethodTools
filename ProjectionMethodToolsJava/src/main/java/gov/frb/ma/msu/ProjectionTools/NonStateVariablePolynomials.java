package gov.frb.ma.msu.ProjectionTools;
import Jama.Matrix;
/**
 * Contains Information describing the non state variables and the state variables which upon which they are based
 * @author m1gsa00
 *
 */
public class NonStateVariablePolynomials 
{
	
	
/**
 * 	
 * @return String[] state variable names
 */
	public String[] getNonStateVariableNames() {
		return nonStateVariableNames;
	}
	public void setNonStateVariableNames(String[] varNames) {
		this.nonStateVariableNames = varNames;
		this.setNonStateVarDim(varNames.length);
	}

	protected String[] nonStateVariableNames;
	
    private int nonStateVarDim=0;
    
    /**
     * returns the number of non state variables
     * @return
     */
	public int getNonStateVarDim() {
		return nonStateVarDim;
	}
	public void setNonStateVarDim(int nonStateVarDim) {
		this.nonStateVarDim = nonStateVarDim;
	}
	public NonStateVariablePolynomials( 
		  String[] someVarNames) throws ProjectionRuntimeException {
		  	setNonStateVariableNames(someVarNames);
		  }  

 /**
  * returns the non state variable names  
  * @return String []
  */
	public String [] getVariableNamesNSP() {
		return(getNonStateVariableNames());
		}
	    
  
 


  

/**
 * true if the name provided is among the non state variable names
 * @param theVar 
 * @see VarTime
 * @return boolean
 */
    	public boolean hasVarQNSP(VarTime theVar) {
    	String varName=theVar.getVarName();
    	try {varPositionNSP(varName);return(true);}
    	catch(ProjectionRuntimeException ee) { return(false);}
    	}
/**
 * returns the variable number of the provided variable name if it is among the non state variable names
 * @param theVar
 * @return int 
 * @throws ProjectionRuntimeException
 */
    	public int varPositionNSP(String theVar) throws ProjectionRuntimeException {
    		int ii;
    		int thePos=-1;
    		for(ii=0;ii<getVariableNamesNSP().length;ii++){
    		    if(getVariableNamesNSP()[ii].equals(theVar)) {thePos=ii;break;}
    		}
    	    if(thePos<0) throw new 
    	ProjectionRuntimeException("varPosition: var name not in CollocationPolynomial");
    		return(thePos);
    	    }
	




		private Matrix relevantWeightsNSP;
		
/**
 * returns the polynomial weights for the non state variables
 * rows ordered by variable name as given in nonStateVariableNames and provided by varPosition
 * columns use ProjectionMethodTools order 
 * @see GridPointsSpec
 * @return
 */
			public Matrix getRelevantWeightsNSP() {
				return relevantWeightsNSP;
			}
			public void setRelevantWeightsNSP(Matrix relevantWeightsNSP) {
				this.relevantWeightsNSP = relevantWeightsNSP;
			}

			
}
