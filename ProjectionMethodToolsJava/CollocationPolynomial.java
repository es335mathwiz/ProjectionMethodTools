package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;
/**
 * Container for information characterizing the details of the collocating 
 * polynomial points.
 * 
 * @author Gary S Anderson (m1gsa00)
 *
 */
public  class CollocationPolynomial {
	GridPointsSpec theGrid;
	private Matrix variablePolynomialWeights;






		
	


	public GridPointsSpec getTheGrid() {
		return theGrid;
	}

	public void setTheGrid(GridPointsSpec theGrid) {
		this.theGrid = theGrid;
	}

	public String [] getStateVariableNames() {
	return(getTheGrid().getVariableNames());
	}
	public String [] getVariableNames() {
		return(getStateVariableNames());
		}

	public int [] getOrders() {return(getTheGrid().getOrders());}

	public double [] [] getRanges() {
		return(getTheGrid().getRanges());
	}
  	
  public void setVariablePolynomialWeights(Matrix variablePolynomialWeights) {
		this.variablePolynomialWeights = variablePolynomialWeights;
	}

public Matrix  getStateVariablePolynomialWeights() {
	return(variablePolynomialWeights/*.getArrayCopy()*/);
    }

	public double [][] getBasisAtChebNodes() {
	return(getTheGrid().getBasisAtChebNodes());
	}

	public double [][] getXformedChebNodePts() {
	return(getTheGrid().getTheChebPoly().getXformedOuterProdEvalPoints());
	}

	public double [] getXformedChebNodePts(String varName) throws ProjectionRuntimeException {
	return(getTheGrid().getXformedChebNodePts(varName));
	} 

	public int getVarIndex(String varName) throws ProjectionRuntimeException {
	return(getTheGrid().getVarIndex(varName));
	}



	public Matrix getBasisAtChebNodesAsMatrix() {
		return getTheGrid().getBasisAtChebNodesAsMatrix();
	}

	public void setBasisAtChebNodesAsMatrix() {
		getTheGrid().setBasisAtChebNodesAsMatrix();
	}

	public void setBasisAtChebNodesAsMatrix(Matrix aMat) {
		getTheGrid().setBasisAtChebNodesAsMatrix(aMat);
	}

	public int  numberOfStateVars(){
	    	return(getTheGrid().numberOfGridVars());
	 
	   }
	    





		public ChebyshevPolysAtEvalPoints getTheChebyshevPolysAtEvalPoints() {
			return getTheGrid().getTheChebPoly();
		}



		public int getNumPolys() {
			return getTheGrid().powersPlusOneProd();
		}



		public int getStateDimWithoutShocks() {
			return getTheGrid().getStateDim()-getTheGrid().getNumberOfShocks();
		}




		public int getNumberOfShocks() {return(getTheGrid().getNumberOfShocks());
		}

		public void setNumberOfShocks(int val) {getTheGrid().setNumberOfShocks(val);}
	
	    }
