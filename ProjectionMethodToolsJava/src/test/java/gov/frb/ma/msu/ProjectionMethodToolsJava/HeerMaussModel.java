package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ShockVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class HeerMaussModel extends DoEqns {
HeerMaussModel meFunc(){return(this);}
double [] theShocks = new double[0];

double useShock(final int loc, final StochasticBasis theStochasticBasis){
if(theStochasticBasis.isPerfectForesightQ()) return(0);else {
return(theShocks[loc]);}}
final void doShock(final int loc,final double val){theShocks[loc]=val;}
public void updateParams(double[] paramVec){

}
 public double[] getParams(){ double [] paramVec = new double[0]; return(paramVec);
}

    public EquationValDrv updateValDrv(final StochasticBasis theStochasticBasis) throws ProjectionRuntimeException{
StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
VT=new StateVarTime("kk",-1);

 final EquationValDrv kk$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",-1);

 final EquationValDrv zz$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("kk",0); final EquationValDrv kk$t=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",0); final EquationValDrv zz$t=VT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("cc",0); final EquationValDrv cc$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("lambda",0); final EquationValDrv lambda$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("nn",0); final EquationValDrv nn$t=NVT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",1); final EquationValDrv zz$tp1=VT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("lambda",1); final EquationValDrv lambda$tp1=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("nn",1); final EquationValDrv nn$tp1=NVT.evalVar(theStochasticBasis);


gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theStochasticBasis.getTheGaussHermite();

Integrand forInt20$Func = new Integrand(){
public int epsDim(){return(0);};
public int numVars(){return(0);};
public int numNodes(){return(0);};
public void setShocks(StochasticBasis theStochasticBasis,double[]epsVec){
int[] newLocs={};
int [] theSVLocs={};
theStochasticBasis.setShockVarLocs(theSVLocs);
theStochasticBasis.setTheShockVals(epsVec);

theStochasticBasis.updateForShocks(meFunc(),newLocs,epsVec);}
public EquationValDrv evaluate(double[]epsVec,StochasticBasis theStochasticBasis)
throws ProjectionRuntimeException{
setShocks(theStochasticBasis,epsVec);
StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
VT=new StateVarTime("kk",-1);

 final EquationValDrv kk$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",-1);

 final EquationValDrv zz$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("kk",0); final EquationValDrv kk$t=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",0); final EquationValDrv zz$t=VT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("cc",0); final EquationValDrv cc$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("lambda",0); final EquationValDrv lambda$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("nn",0); final EquationValDrv nn$t=NVT.evalVar(theStochasticBasis);

VT=new StateVarTime("zz",1); final EquationValDrv zz$tp1=VT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("lambda",1); final EquationValDrv lambda$tp1=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("nn",1); final EquationValDrv nn$tp1=NVT.evalVar(theStochasticBasis);


EquationValDrv forInt20=kk$t.pow(-0.64).times(lambda$tp1.times(-0.3544505221282943)).times(nn$tp1.pow(0.64)).times(zz$tp1.exp()).plus(lambda$tp1.times(-0.9599701640974638));;
return(forInt20);}};

EquationValDrv forInt20=gh.integrate(forInt20$Func,theStochasticBasis);


EquationValDrv eqn78=cc$t.pow(-1).plus(lambda$t.times(-1));
EquationValDrv eqn79=kk$tm1.pow(0.36).times(lambda$t.times(-0.64)).times(nn$t.pow(-0.36)).times(zz$t.exp()).plus(nn$t.times(-1).plus(1).pow(-1).times(-0.05));
EquationValDrv eqn80=cc$t.plus(kk$t.times(0.030500000000000083)).plus(kk$tm1.pow(0.36).times(nn$t.pow(0.64)).times(zz$t.exp()).times(-1));
EquationValDrv eqn81=lambda$t.plus(forInt20);
EquationValDrv eqn82=zz$t.plus(zz$tm1.times(-0.95));

EquationValDrv sys=eqn78.augSys(eqn79).augSys(eqn80).augSys(eqn81).augSys(eqn82);
return(sys);}}

