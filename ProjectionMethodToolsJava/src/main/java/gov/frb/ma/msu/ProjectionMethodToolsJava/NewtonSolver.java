package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;


public class NewtonSolver {
	private int iLower,iUpper,jLower,jUpper;
private boolean undefinedRanges=true;
	public boolean isundefinedRanges() {
	return undefinedRanges;
}

public void setundefinedRanges(boolean undefinedRangesV) {
	undefinedRanges = undefinedRangesV;
}

	public int getiLower() {
		return iLower;
	}

	public void setiLower(int iLower) {
		this.iLower = iLower;
	}

	public int getiUpper() {
		return iUpper;
	}

	public void setiUpper(int iUpper) {
		this.iUpper = iUpper;
	}

	public int getjLower() {
		return jLower;
	}

	public void setjLower(int jLower) {
		this.jLower = jLower;
	}

	public int getjUpper() {
		return jUpper;
	}

	public void setjUpper(int jUpper) {
		this.jUpper = jUpper;
	}
	double shrinkFactor=0.7;double minShrink=1.0e-5;double shrinkNow=1;
	 public double getShrinkFactor() {
		return shrinkFactor;
	}
	public void setShrinkFactor(double shrinkFactor) {
		this.shrinkFactor = shrinkFactor;
	}
	double newtonMethodEpsilon = 1e-7;
	    int newtonMethodMaxIterations = 120;
	    boolean recordingProgressQ=true;
	public boolean isRecordingProgressQ() {
			return recordingProgressQ;
		}
    NewtonIterSequenceInfo newtonIters=new NewtonIterSequenceInfo(
			shrinkFactor,minShrink,
			newtonMethodEpsilon, newtonMethodMaxIterations);


		public NewtonIterSequenceInfo getNewtonIters() {
		return newtonIters;
	}
	public void setNewtonIters(NewtonIterSequenceInfo newtonIters) {
		this.newtonIters = newtonIters;
	}
	public void newtonItersAdd(NewtonIterInfo nii,double shrinkWD){
		if(isRecordingProgressQ()){newtonIters.add(nii);newtonIters.setShrinkWhenDone(shrinkWD);}
	}

	
	
	
		public NewtonSolver() {
		super();
	}
		public NewtonSolver(int iLow,int iHigh,int jLow, int jHigh){
			setiLower(iLow);setiUpper(iHigh);
			setjLower(jLow);setjUpper(jHigh);
			setundefinedRanges(false);	
		}
		public NewtonSolver(double shrinkFactor, double minShrink,
			double shrinkNow, double newtonMethodEpsilon,
			int newtonMethodMaxIterations, boolean recordingProgressQ,
			NewtonIterSequenceInfo newtonIters) {
		super();
		setundefinedRanges(true);
		this.shrinkFactor = shrinkFactor;
		this.minShrink = minShrink;
		this.shrinkNow = shrinkNow;
		this.newtonMethodEpsilon = newtonMethodEpsilon;
		this.newtonMethodMaxIterations = newtonMethodMaxIterations;
		this.recordingProgressQ = recordingProgressQ;
		this.newtonIters = newtonIters;
	}
		public void setRecordingProgressQ(boolean recordingProgressQ) {
			this.recordingProgressQ = recordingProgressQ;
		}
	public  double [][] solveWSB(WeightedStochasticBasis solution, double[][] xx, DoEqns modelEquations) 
	throws ProjectionRuntimeException{
	int itsSoFar=0;
	if(undefinedRanges){
	solution.setAllWeights(xx);}
	else {
		int rowDim=/*solution.theNonState.getTheGrid().powersPlusOneProd()*/(solution.getTheNonState().getNonStateVarDim()+solution.getTheState().getStateVariableNames().length);
		int colDim=xx[0].length;
		double [][]bigX= new double [rowDim][colDim];
		int ii,jj;
		for(ii=0;ii<=getiUpper()-getiLower();ii++){
			for(jj=0;jj<colDim;jj++){
				bigX[ii+getiLower()][jj]=xx[ii][jj];
			}
		}
		xx=bigX;
		solution.setAllWeights(xx);}

	EquationValDrv theEqValDrv=modelEquations.updateValDrv(solution);
    if ( theEqValDrv.theVal.norm1()<newtonMethodEpsilon) newtonIters.setNewtonConvergedQ(true);
    else{
	    while (true) {    itsSoFar++;solution.setNewtonSteps(itsSoFar);
	    Matrix delta = computeDelta(solution, theEqValDrv);
	    setShrinkNow(1);
		takeFeasibleStep(solution, xx, theEqValDrv, delta,modelEquations);
	
		
		solution.setAllWeights(xx);
	    theEqValDrv=modelEquations.updateValDrv(solution);
	        if (delta.norm1() < newtonMethodEpsilon|| theEqValDrv.theVal.norm1()<newtonMethodEpsilon) break;
	    checkWhetherMaxIterationsExceeded(itsSoFar, solution, delta,theEqValDrv);
	    }
	    newtonIters.setNewtonConvergedQ(true);
    }
	if(undefinedRanges){
		return xx;
		}else {
			int lilDim=getiUpper()-getiLower()+1,ii;
			int colDim=xx[0].length;int jj;
			double [][] lilxx= new double [lilDim][colDim];
			for(ii=0;ii<lilDim;ii++)
				for(jj=0;jj<colDim;jj++){
				lilxx[ii][jj]=xx[ii+getiLower()][jj];}
	return lilxx;
		}
	}
	
    //final double EPSILON = 1e-14;
    private Matrix delta;
    private  double alpha =1;
	    Matrix compDelta(EquationValDrv theEqns) {
	            Matrix JJ = theEqns.getTheJac();
	            delta = JJ.inverse().times(theEqns.getTheVal());
		return(delta);
	    }
		public double [] solveSS(SSEqns theEqns,double[]initGuess){
			EquationValDrv valsNow=theEqns.getSsEvaler().updateSSGuess(initGuess);
			double [] guessNow=initGuess;
			Matrix ff=valsNow.getTheVal();
	Matrix delta=new Matrix(ff.getRowDimension(),ff.getColumnDimension());
		        while (true) {
			    try{
				delta=compDelta(valsNow);
		            if (delta.norm1() < getNewtonMethodEpsilon()) break;
			    } catch (Exception ee) { alpha=alpha/2;delta=compDelta(valsNow);}
			    guessNow=arrayDiff(guessNow,delta.getColumnPackedCopy());
			    valsNow=theEqns.getSsEvaler().updateSSGuess(guessNow);
		        }
		        return guessNow;
		    }
		
	double [] arrayDiff(double[] fir,double[] sec){
		int ii;
		double [] res=new double[fir.length];
		for(ii=0;ii<fir.length;ii++){
			res[ii]=fir[ii]-sec[ii];
		}
		return(res);
	}
	
	
	
	
	public double [][] solveAtPts(EquateAtGridPoints rEqns,double[][] xx){
return(	this.solveWSB(rEqns.getValuesToEquateModel(), xx, rEqns));
			}
	
	private Matrix computeDelta(Basis solution,
			EquationValDrv theEqValDrv) {
		Matrix JJAll,JJ,delta;
		JJAll = theEqValDrv.obtainJacobian(solution);
		if(undefinedRanges){
	    delta = JJAll.solve(theEqValDrv.theVal);
		}else {
			int lilNumRows= theEqValDrv.obtainJacobian(solution).getRowDimension();
			int numPolys=solution.getTheState().getTheGrid().powersPlusOneProd();
			int bigNumRows=(solution.getStateVarDim()+solution.getNonStateVarDim())*numPolys;
			JJ = theEqValDrv.obtainJacobian(solution).getMatrix(0,lilNumRows-1,getjLower(),getjUpper());
		    delta = new Matrix(bigNumRows,1); 
	    delta.setMatrix(getiLower()*numPolys,(getiUpper()+1)*numPolys-1,0,0,JJ.solve(theEqValDrv.theVal.getMatrix(0,lilNumRows-1,0,0)));

			      }
		return delta;
	}
	void takeFeasibleStep(WeightedStochasticBasis solution, double[][] xx,
			EquationValDrv theEqValDrv, Matrix delta,DoEqns modelEquations) {
		Utilities.chkNan(xx,"xx");
		Utilities.chkNan(theEqValDrv.theVal.getArray(),"theVal");
		try{
		modelEquations.noNans(solution,xx,delta.getArray());
		newtonItersAdd(new NewtonIterInfo(xx,delta.getArray(),getShrinkNow(),theEqValDrv.getTheVal().getArray(),theEqValDrv.getTheJac().getArray(), true),shrinkNow);}
	 catch (ProjectionRuntimeException ee) {
			newtonItersAdd(new NewtonIterInfo(xx,delta.getArray(),getShrinkNow(),theEqValDrv.getTheVal().getArray(),theEqValDrv.getTheJac().getArray(), false),shrinkNow);
		if(getShrinkNow()<getMinShrink()){
    	throw new ProjectionRuntimeException("from NewtonSolver shrinker:"+ee.getMessage());} 
     else {
	 setShrinkNow(getShrinkFactor()*getShrinkNow());
	 takeFeasibleStep(solution, xx, theEqValDrv, delta.times(getShrinkNow()),modelEquations);}}
		return;
	}

	
	public double getMinShrink() {
		return minShrink;
	}
	public void setMinShrink(double minShrink) {
		this.minShrink = minShrink;
	}
	public double getShrinkNow() {
		return shrinkNow;
	}
	public void setShrinkNow(double shrinkNow) {
		this.shrinkNow = shrinkNow;
	}
	void checkWhetherMaxIterationsExceeded(int itsSoFar, Basis solution, Matrix delta,
			EquationValDrv theEqValDrv) {
		if(itsSoFar>newtonMethodMaxIterations){
			String eStr;
		 eStr="CollocationSolution:root newtonMethodMaxIterations exceeded";
		eStr=eStr+ "norm(delta)=" +delta.norm1();
		eStr=eStr+ "norm(theVal)=" +theEqValDrv.theVal.norm1();
		throw new ProjectionRuntimeException(eStr);}
	} 

	public  double getNewtonMethodEpsilon() {
		return newtonMethodEpsilon;
	}
	public  int getNewtonMethodMaxIterations() {
		return newtonMethodMaxIterations;
	}
	public void setNewtonMethodEpsilon(double epsilon) {
		newtonMethodEpsilon = epsilon;
	}
	public void setNewtonMethodMaxIterations(int maxits) {
		newtonMethodMaxIterations = maxits;
	}
}
