package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class doOneNS extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;NonStateVarTime nVT;
kVT=new StateVarTime("kk",-1);
kVT=new StateVarTime("kk",0);
EquationValDrv k0 = kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",1);
nVT=new NonStateVarTime("aName1",0);
EquationValDrv an10 =nVT.evalVar(theProjModel);
/*
kVT=new StateVarTime("aName1",1);
eqValDrv an1p1 =kVT.evalVar(theCollocationSolution);
*/
EquationValDrv k0t0 = k0.minus(5).augSys(an10.minus(3));
return(k0t0);
    }
    public	double [] getParams(){return (new double[0]); };
}
