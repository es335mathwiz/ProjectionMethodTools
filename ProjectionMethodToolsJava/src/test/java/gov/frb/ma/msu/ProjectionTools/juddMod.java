package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.Integrand;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;
 public class juddMod extends DoEqns {
 juddMod meFunc(){return(this);}
double [] theShocks = new double[0];

double useShock(final int loc, final StochasticBasis theProjModel){
if(theProjModel.isPerfectForesightQ()) return(0);else {
return(theShocks[loc]);}}
final void doShock(final int loc,final double val){theShocks[loc]=val;}
public void updateParams(double[] paramVec){

}
 public double[] getParams(){ double [] paramVec = new double[0]; return(paramVec);
}

    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("kk",-1); final EquationValDrv kk$tm1=VT.evalVar(theProjModel);

VTN=new NonStateVarTime("cc",0); final EquationValDrv cc$t=VTN.evalVar(theProjModel);

VT=new StateVarTime("kk",0); final EquationValDrv kk$t=VT.evalVar(theProjModel);

VTN=new NonStateVarTime("cc",1); final EquationValDrv cc$tp1=VTN.evalVar(theProjModel);


gov.frb.ma.msu.ProjectionTools.GaussHermite gh = theProjModel.getTheGaussHermite();

Integrand forInt1$Func = new Integrand(){
public int epsDim(){return(0);};
public int numVars(){return(0);};
public int numNodes(){return(0);};
public void setShocks(StochasticBasis theProjModel,double[]epsVec){
int[] newLocs={};

theProjModel.updateForShocks(meFunc(),newLocs,epsVec);}
public EquationValDrv evaluate(double[]epsVec,StochasticBasis theProjModel)
throws ProjectionRuntimeException{
setShocks(theProjModel,epsVec);EquationValDrv forInt1=cc$tp1.pow(-0.9).times(kk$t.pow(-0.75)).times(-0.9999999999999999);;
return(forInt1);}};

EquationValDrv forInt1=gh.integrate(forInt1$Func,theProjModel);


EquationValDrv eqn1=cc$t.plus(kk$t).plus(kk$tm1.pow(0.25).times(-4.2105263157894735));
EquationValDrv eqn2=cc$t.pow(-0.9).plus(forInt1);

EquationValDrv sys=eqn1.augSys(eqn2);
return(sys);}}






















