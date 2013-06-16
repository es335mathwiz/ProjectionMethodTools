package gov.frb.ma.msu.ProjectionMethodToolsJava;

public class FindZeroAbstractClass implements FindZeroStrategyInterface{

	StrategyIterSequenceInfo strategyItersSeq;
	
	public FindZeroAbstractClass() {
		super();
		strategyItersSeq = new StrategyIterSequenceInfo();
	}

	public StrategyIterSequenceInfo getStrategyItersSeq() {
		return strategyItersSeq;
	}

	public void setStrategyItersSeq(StrategyIterSequenceInfo newtonIS) {
		this.strategyItersSeq = newtonIS;
	}

	public double [][] findCollocationWeights(WeightedStochasticBasis cs, double [][] initWts, DoEqns theSys,double fracNow)
			throws ProjectionRuntimeException {
	
			double [][] theRes;
			NewtonSolver aSolver=new NewtonSolver();
			try{
			theRes=aSolver.solveWSB(cs,initWts,theSys);
			strategyItersSeq.add(aSolver.getNewtonIters());
			return(theRes);
			} catch(Exception ee){
			strategyItersSeq.add(aSolver.getNewtonIters());
			throw new ProjectionRuntimeException("from FindZeroAbstractClass:"+ee.getMessage());}

}}