package gov.frb.ma.msu.ProjectionTools;

public abstract class SSEvaluater extends Evaluater {


	public abstract EquationValDrv updateSSGuess(double [] initGuess);
	public abstract int getNumSSVals();





}