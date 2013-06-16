package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ShockVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
 public class SimpleShockForTests extends DoEqns {
 SimpleShockForTests meFunc(){return(this);}
 double [] theShocks = new double[2];

 double useShock(final int loc, final StochasticBasis theCollocationSolution){
 if(theCollocationSolution.isPerfectForesightQ()) return(0);else {
 return(theShocks[loc]);}}
 final void doShock(final int loc,final double val){theShocks[loc]=val;}
 public void updateParams(double[] paramVec){

 }
  public double[] getParams(){ double [] paramVec = new double[0]; return(paramVec);
 }

     public EquationValDrv updateValDrv(final StochasticBasis theCollocationSolution) throws ProjectionRuntimeException{
 StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
 VT=new StateVarTime("AA",-1);

  final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

 VT=new StateVarTime("PP",-1);

  final EquationValDrv PP$tm1=VT.evalVar(theCollocationSolution);

 VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

 VT=new StateVarTime("PP",0); final EquationValDrv PP$t=VT.evalVar(theCollocationSolution);

 NVT=new NonStateVarTime("PPNxt",0); final EquationValDrv PPNxt$t=NVT.evalVar(theCollocationSolution);

 NVT=new NonStateVarTime("PPSqrd",0); final EquationValDrv PPSqrd$t=NVT.evalVar(theCollocationSolution);


 VTS=new ShockVarTime("AA$Shock",-1); final EquationValDrv AA$Shock$tm1=VTS.evalVar(theCollocationSolution);

 VTS=new ShockVarTime("PP$Shock",-1); final EquationValDrv PP$Shock$tm1=VTS.evalVar(theCollocationSolution);

 gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theCollocationSolution.getTheGaussHermite();

 Integrand forInt1$Func = new Integrand(){
 public int epsDim(){return(2);};
 public int numVars(){return(0);};
 public int numNodes(){return(0);};
 public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
 int[] newLocs={2, 3};
 int [] theSVLocs={-1, -1, 0, 1};
 theCollocationSolution.setShockVarLocs(theSVLocs);
theCollocationSolution.setTheShockVals(epsVec);
 doShock(0,epsVec[0]);
 doShock(1,epsVec[1]);
 theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
 public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
 throws ProjectionRuntimeException{
 setShocks(theCollocationSolution,epsVec);
NonStateVarTime NVT;
 


 NVT=new NonStateVarTime("PPNxt",1); final EquationValDrv PPNxt$tp1=NVT.evalVar(theCollocationSolution);

 EquationValDrv forInt1=PPNxt$tp1.pow(3).times(-1);;
 return(forInt1);}};

 EquationValDrv forInt1=gh.integrate(forInt1$Func,theCollocationSolution);

 
 EquationValDrv eqn1=AA$Shock$tm1.times(-1).plus(AA$t).plus(AA$tm1.times(-0.5));
 EquationValDrv eqn2=PP$Shock$tm1.times(-1).plus(PP$t).plus(PP$tm1.times(-0.7));
 EquationValDrv eqn3=PPNxt$t.plus(PP$Shock$tm1.times(-1));
 EquationValDrv eqn4=PPSqrd$t.plus(forInt1);

 EquationValDrv sys=eqn1.augSys(eqn2).augSys(eqn3).augSys(eqn4);
 return(sys);}}
