package gov.frb.ma.msu.gsmin;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class  experTackCnstrns extends DoEqns{
public double alpha;
public double anEps;
 public double get$alpha() { return(alpha);}
 public double get$anEps() { return(anEps);}
public void set$alpha(double theVal) { alpha=theVal;}
public void set$anEps(double theVal) { anEps=theVal;}
public void updateParams(double[] paramVec){
set$alpha(paramVec[0]);
set$anEps(paramVec[1]);

}
 public double[] getParams(){ double [] paramVec = new double[2];paramVec[0]=get$alpha();
paramVec[1]=get$anEps();
 return(paramVec);
}

    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theProjModel);

VT=new StateVarTime("AA$Shock",0); final EquationValDrv AA$Shock$t=VT.evalVar(theProjModel);

VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theProjModel);

VTN=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("CC",0); final EquationValDrv CC$t=VTN.evalVar(theProjModel);

VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theProjModel);

VT=new StateVarTime("GG$Shock",0); final EquationValDrv GG$Shock$t=VT.evalVar(theProjModel);

VTN=new NonStateVarTime("HH",0); final EquationValDrv HH$t=VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("bigPi",1); 


/*eqValDrv forInt29=gh.integrate(forInt29$Func,theCollocationSolution);*/
EquationValDrv eqn71=AA$t.times(-1).plus(1.);
EquationValDrv eqn72=AA$t.plus(-3.);
//eqValDrv eqn73=AA$tp1.times(-1).plus(1.);
//eqValDrv eqn74=AA$tp1.plus(-3.);
EquationValDrv eqn75=AA$Shock$t.times(-1).plus(-1.);
EquationValDrv eqn76=AA$Shock$t.plus(-1.);
//eqValDrv eqn77=AA$Shock$tp1.times(-1).plus(-1.);
//eqValDrv eqn78=AA$Shock$tp1.plus(-1.);
EquationValDrv eqn79=bigDelta$t.times(-1).plus(1.);
EquationValDrv eqn80=bigDelta$t.plus(-3.);
//eqValDrv eqn81=bigDelta$tp1.times(-1).plus(1.);
//eqValDrv eqn82=bigDelta$tp1.plus(-3.);
EquationValDrv eqn83=bigPi$t.times(-1);
EquationValDrv eqn84=bigPi$t.pow(-1 + anEps).times(-1).times(1/(-1 + alpha)).times(alpha).plus(1/(-1 + alpha));
//eqValDrv eqn85=bigPi$tp1.times(-1);
EquationValDrv eqn86=CC$t.times(-1);
EquationValDrv eqn87=GG$t.times(-1).plus(1.);
EquationValDrv eqn88=GG$t.plus(-3.);
//eqValDrv eqn89=GG$tp1.times(-1).plus(1.);
//eqValDrv eqn90=GG$tp1.plus(-3.);
EquationValDrv eqn91=GG$Shock$t.times(-1).plus(-1.);
EquationValDrv eqn92=GG$Shock$t.plus(-1.);
//eqValDrv eqn93=GG$Shock$tp1.times(-1).plus(-1.);
//eqValDrv eqn94=GG$Shock$tp1.plus(-1.);
EquationValDrv eqn95=HH$t.times(-1);

EquationValDrv sys=eqn71.augSys(eqn72)/*.augSys(eqn73).augSys(eqn74)*/.augSys(eqn75).augSys(eqn76)/*.augSys(eqn77).augSys(eqn78)*/.augSys(eqn79).augSys(eqn80)/*.augSys(eqn81).augSys(eqn82)*/.augSys(eqn83).augSys(eqn84)/*.augSys(eqn85)*/.augSys(eqn86).augSys(eqn87).augSys(eqn88)/*.augSys(eqn89).augSys(eqn90)*/.augSys(eqn91).augSys(eqn92)/*.augSys(eqn93).augSys(eqn94)*/.augSys(eqn95);
return(sys);}}	