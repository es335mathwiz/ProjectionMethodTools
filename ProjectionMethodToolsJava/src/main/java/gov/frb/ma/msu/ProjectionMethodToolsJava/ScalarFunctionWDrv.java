package gov.frb.ma.msu.ProjectionMethodToolsJava;
public interface ScalarFunctionWDrv {
    public double[] evaluate(double[] dd) throws Exception ;
    public double [][] evaluate(double [][] dd) throws Exception ;
    public double [][] evaluate() throws Exception ;
    public double [][] evaluateDrv(double[] dd) throws Exception ;
    public double [][][] evaluateDrv(double [][] dd) throws Exception ;
    public double [][][] evaluateDrv() throws Exception ;
}
