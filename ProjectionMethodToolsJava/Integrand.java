package gov.frb.ma.msu.ProjectionTools;
public interface Integrand {
    public EquationValDrv evaluate(double [] dd, StochasticBasis theProj) throws ProjectionRuntimeException ;
    /*   public eqValDrv [] evaluate(double [] dd, CollocationSolution theProj) throws projectionRuntimeException ;
	 public eqValDrv [][] evaluateDrv(double [][] dd, CollocationSolution theProj) throws projectionRuntimeException ;*/
    public int epsDim();
    public int numNodes();
    public int numVars();
    public void setShocks(StochasticBasis pm,double[] eps);
}
