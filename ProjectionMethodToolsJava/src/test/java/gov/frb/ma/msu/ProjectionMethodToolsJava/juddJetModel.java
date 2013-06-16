package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
 public class juddJetModel extends DoEqns {
 juddJetModel meFunc(){return(this);}
double [] theShocks = new double[0];

double useShock(final int loc, final StochasticBasis theCollocationSolution){
if(theCollocationSolution.isPerfectForesightQ()) return(0);else {
return(theShocks[loc]);}}
final void doShock(final int loc,final double val){theShocks[loc]=val;}
public void updateParams(double[] paramVec){

}
 public double[] getParams(){ double [] paramVec = new double[0]; return(paramVec);
}

    public EquationValDrv updateValDrv(StochasticBasis theCollocationSolution) throws ProjectionRuntimeException{
    	StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("kk",-1); final EquationValDrv kk$tm1=VT.evalVar(theCollocationSolution);

VTN=new NonStateVarTime("cc",0); final EquationValDrv cc$t=VTN.evalVar(theCollocationSolution);

VT=new StateVarTime("kk",0); final EquationValDrv kk$t=VT.evalVar(theCollocationSolution);

VTN=new NonStateVarTime("cc",1); final EquationValDrv cc$tp1=VTN.evalVar(theCollocationSolution);


gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theCollocationSolution.getTheGaussHermite();

Integrand forInt1$Func = new Integrand(){
public int epsDim(){return(0);};
public int numVars(){return(0);};
public int numNodes(){return(0);};
public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
int[] newLocs={};

theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
throws ProjectionRuntimeException{
setShocks(theCollocationSolution,epsVec);EquationValDrv forInt1=cc$tp1.pow(-0.9).times(kk$t.pow(-0.75)).times(-0.9999999999999999);;
return(forInt1);}};

EquationValDrv forInt1=gh.integrate(forInt1$Func,theCollocationSolution);


EquationValDrv eqn1=cc$t.plus(kk$t).plus(kk$tm1.pow(0.25).times(-4.2105263157894735));
EquationValDrv eqn2=cc$t.pow(-0.9).plus(forInt1);

EquationValDrv sys=eqn1.augSys(eqn2);
return(sys);}}