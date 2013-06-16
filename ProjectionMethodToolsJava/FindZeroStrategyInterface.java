package gov.frb.ma.msu.ProjectionTools;

public interface FindZeroStrategyInterface {

	public abstract StrategyIterSequenceInfo getStrategyItersSeq();

	public abstract void setStrategyItersSeq(StrategyIterSequenceInfo StrategyIters);

	public abstract double[][] findCollocationWeights(WeightedStochasticBasis cs,
			double[][] initWts, DoEqns theSys, double fracWidth)
			throws ProjectionRuntimeException;

}