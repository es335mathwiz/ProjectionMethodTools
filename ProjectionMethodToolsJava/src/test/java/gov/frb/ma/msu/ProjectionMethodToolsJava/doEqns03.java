package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class doEqns03 extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;NonStateVarTime nVT;
kVT=new StateVarTime("kk",-1);
kVT=new StateVarTime("kk",0);
EquationValDrv k0 = kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",1);
kVT=new StateVarTime("theta",-1);
kVT=new StateVarTime("theta",0);
EquationValDrv t0 =kVT.evalVar(theProjModel);
kVT=new StateVarTime("theta",1);
nVT=new NonStateVarTime("aName1",0);
EquationValDrv an10 =nVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName2",0);
EquationValDrv an20 =nVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName3",0);
EquationValDrv an30 =nVT.evalVar(theProjModel);
//eqValDrv k0t0 = k0.times(k0.plus(km1.times(-.1))).minus(0.0002).augSys(t0.plus(0.1)).times(0.2);
EquationValDrv k0t0 = 
    an10.log().minus(.2).augSys(t0.plus(3.5)).augSys(an20).augSys(an30).augSys(k0.minus(5));
return(k0t0);
    }
    public	double [] getParams(){return (new double[0]); };
}
