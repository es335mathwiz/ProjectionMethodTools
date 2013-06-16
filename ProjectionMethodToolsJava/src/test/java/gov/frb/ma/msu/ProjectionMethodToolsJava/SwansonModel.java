package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;



public class SwansonModel extends DoEqns {
SwansonModel meFunc(){return(this);}
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
StateVarTime VT;NonStateVarTime NVT;
VT=new StateVarTime("A",-1);

final EquationValDrv A$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("K",-1);

final EquationValDrv K$tm1=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("A",0); final EquationValDrv A$t=VT.evalVar(theStochasticBasis);

VT=new StateVarTime("K",0); final EquationValDrv K$t=VT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("C",0); final EquationValDrv C$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("Inv",0); final EquationValDrv Inv$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("r",0); final EquationValDrv r$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("Welf",0); final EquationValDrv Welf$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("Y",0); final EquationValDrv Y$t=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("C",1); 

NVT=new NonStateVarTime("r",1); 

NVT=new NonStateVarTime("Welf",1); 


gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theStochasticBasis.getTheGaussHermite();

Integrand forInt24$Func = new Integrand(){
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
NonStateVarTime NVT;
NVT=new NonStateVarTime("C",0); 

NVT=new NonStateVarTime("Inv",0); 

NVT=new NonStateVarTime("r",0); 

NVT=new NonStateVarTime("Welf",0); 

NVT=new NonStateVarTime("Y",0); 

NVT=new NonStateVarTime("C",1); final EquationValDrv C$tp1=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("r",1); final EquationValDrv r$tp1=NVT.evalVar(theStochasticBasis);

NVT=new NonStateVarTime("Welf",1); 


EquationValDrv forInt24=C$tp1.exp().pow(-1.1).times(-0.99).plus(C$tp1.exp().pow(-1.1).times(r$tp1.times(-0.99)));;
return(forInt24);}};

EquationValDrv forInt24=gh.integrate(forInt24$Func,theStochasticBasis);


Integrand forInt25$Func = new Integrand(){
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
NonStateVarTime NVT;
NVT=new NonStateVarTime("C",0); 

NVT=new NonStateVarTime("Inv",0); 

NVT=new NonStateVarTime("r",0); 

NVT=new NonStateVarTime("Welf",0); 

NVT=new NonStateVarTime("Y",0); 

NVT=new NonStateVarTime("C",1); 

NVT=new NonStateVarTime("r",1); 

NVT=new NonStateVarTime("Welf",1); final EquationValDrv Welf$tp1=NVT.evalVar(theStochasticBasis);


EquationValDrv forInt25=Welf$tp1.times(-0.99);;
return(forInt25);}};

EquationValDrv forInt25=gh.integrate(forInt25$Func,theStochasticBasis);


EquationValDrv eqn95=A$t.exp().times(-1).times(K$tm1.exp().pow(0.3)).plus(Y$t.exp());
EquationValDrv eqn96=A$t.exp().log().plus(A$tm1.exp().log().times(-0.8));
EquationValDrv eqn97=Inv$t.times(-1).plus(K$t.exp()).plus(K$tm1.exp().times(-0.9));
EquationValDrv eqn98=C$t.exp().times(-1).plus(Inv$t.times(-1).plus(Y$t.exp()));
EquationValDrv eqn99=C$t.exp().pow(-1.1).plus(forInt24);
EquationValDrv eqn100=A$t.exp().times(-0.3).times(K$tm1.exp().pow(-0.7)).plus(r$t.plus(0.1));
EquationValDrv eqn101=C$t.exp().pow(-0.10000000000000009).times(9.999999999999991).plus(Welf$t.plus(forInt25));

EquationValDrv sys=eqn95.augSys(eqn96).augSys(eqn97).augSys(eqn98).augSys(eqn99).augSys(eqn100).augSys(eqn101);
return(sys);}}