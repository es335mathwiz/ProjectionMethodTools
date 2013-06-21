package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

//import gov.frb.ma.msu.*;
 public class optModel extends DoEqns {
		public 
		void updateParams(double [] paramVec){};
     public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	 StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("AA",-1); EquationValDrv AA$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("AA",0);EquationValDrv AA$t = VT.evalVar(theProjModel);
VT=new StateVarTime("bigDelta",-1); EquationValDrv bigDelta$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("bigDelta",0);EquationValDrv bigDelta$t = VT.evalVar(theProjModel);VT=new StateVarTime("bigDelta",1);
VTN=new NonStateVarTime("bigPi",0);EquationValDrv bigPi$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("bigPi",1);EquationValDrv bigPi$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("CC",0);EquationValDrv CC$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("CC",1);
VTN=new NonStateVarTime("FF",0);EquationValDrv FF$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("FF",1);EquationValDrv FF$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("HH",0);EquationValDrv HH$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("HH",1);VTN=new NonStateVarTime("SS",0);
EquationValDrv SS$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("SS",1);EquationValDrv SS$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("varphi1",0);EquationValDrv varphi1$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("varphi1",1);
VTN=new NonStateVarTime("varphi2",0);EquationValDrv varphi2$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("varphi2",1);
VTN=new NonStateVarTime("varphi3",0);EquationValDrv varphi3$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("varphi3",1);
VTN=new NonStateVarTime("varphi4",0);EquationValDrv varphi4$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("varphi4",1);EquationValDrv varphi4$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("varphi5",0);EquationValDrv varphi5$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("varphi5",1);
EquationValDrv eqn1=AA$t.times(HH$t).times(varphi2$t).times(0.1).times(bigDelta$t.pow(-1)).times(CC$t.pow(-1)).plus(CC$t.pow(0.1).times(varphi1$t.times(-1)).plus(1));
EquationValDrv eqn2=AA$t.times(varphi1$t).times(-1).times(CC$t.pow(0.1)).plus(AA$t.times(varphi2$t).plus(bigDelta$t.times(CC$t.pow(0.1)).times(HH$t.pow(0.3333333333333333)))).plus(CC$t.pow(0.1).times(HH$t.pow(0.3333333333333333)).times(varphi3$t.times(1.4814814814814814)));
EquationValDrv eqn3=bigPi$t.pow(0.25).times(-0.5).plus(1).pow(4).times(varphi5$t.times(-16)).plus(varphi2$t);
EquationValDrv eqn4=varphi3$t.plus(varphi5$t);
EquationValDrv eqn5=bigDelta$tm1.times(bigPi$t).times(-1).plus(bigPi$t.pow(0.25).times(-1).plus(1).pow(4).times(1.25)).times(varphi4$t).plus(bigPi$t.pow(0.25).times(-0.5).plus(1).pow(-5).times(FF$t.times(varphi5$t).times(-0.0625)));
EquationValDrv eqn6=AA$t.times(HH$t).times(varphi1$t).times(-1).times(bigDelta$t.pow(-2)).plus(AA$t.times(HH$t).times(varphi2$t).times(bigDelta$t.pow(-2)).times(CC$t.pow(-0.1))).plus(bigDelta$t.pow(-2).times(HH$t.pow(1.3333333333333333)).times(varphi3$t.times(1.1111111111111112)).plus(varphi4$t.times(-1))).plus(AA$t.times(HH$t).times(varphi1$t).times(bigDelta$t.pow(-2)).plus(bigDelta$t.pow(-2).times(HH$t.pow(1.3333333333333333)).times(varphi3$t.times(1.1111111111111112))).plus(bigPi$tp1.pow(1.25).times(varphi4$tp1.times(-0.5)).plus(varphi4$t)).plus(AA$t.times(HH$t).times(varphi2$t).times(bigDelta$t.pow(-2)).times(CC$t.pow(-0.1))).times(0.9));
EquationValDrv eqn7=AA$t.log().plus(AA$tm1.log().times(-0.95));
EquationValDrv eqn8=AA$t.times(HH$t).times(-1).times(bigDelta$t.pow(-1)).plus(CC$t);
EquationValDrv eqn9=AA$t.times(HH$t).times(-1).times(bigDelta$t.pow(-1)).times(CC$t.pow(-0.1)).plus(bigPi$tp1.pow(0.25).times(FF$tp1.times(-0.45)).plus(FF$t));
EquationValDrv eqn10=bigDelta$t.pow(-1).times(HH$t.pow(1.3333333333333333)).times(-1.1111111111111112).plus(bigPi$tp1.pow(0.25).times(SS$tp1.times(-0.45))).plus(SS$t);
EquationValDrv eqn11=bigDelta$t.plus(bigDelta$tm1.times(-0.5).times(bigPi$t.pow(1.25))).plus(bigPi$t.pow(0.25).times(-0.5).plus(1).pow(5).times(-0.5));
EquationValDrv eqn12=bigPi$t.pow(0.25).times(-0.5).plus(1).pow(4).times(FF$t.times(-16)).plus(SS$t);

EquationValDrv sys=eqn1.augSys(eqn2).augSys(eqn3).augSys(eqn4).augSys(eqn5).augSys(eqn6).augSys(eqn7).augSys(eqn8).augSys(eqn9).augSys(eqn10).augSys(eqn11).augSys(eqn12);
return(sys);}
     public	double [] getParams(){return (new double[0]); };}
