package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class doTwoNS extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;	NonStateVarTime nVT;
kVT=new StateVarTime("kk",-1);
kVT=new StateVarTime("kk",0);
kVT=new StateVarTime("kk",1);
EquationValDrv kp1 = kVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName1",0);
nVT=new NonStateVarTime("aName1",1);
EquationValDrv an1p1 =nVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName2",0);
nVT=new NonStateVarTime("aName2",1);
EquationValDrv an2p1 =nVT.evalVar(theProjModel);

EquationValDrv k0t0 = 
kp1.minus(5).
augSys(an2p1.minus(2)).
    augSys(an1p1.minus(3));
return(k0t0);
    }
    public	double [] getParams(){return (new double[0]); };
}
