package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class DoMoreElab extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;	NonStateVarTime nVT;
kVT=new StateVarTime("kk",-1);
EquationValDrv km1=kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",0);
EquationValDrv k0 = kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",1);
nVT=new NonStateVarTime("aName1",0);
EquationValDrv an10 =nVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName1",1);
nVT=new NonStateVarTime("aName2",0);
EquationValDrv an20 =nVT.evalVar(theProjModel);
nVT=new NonStateVarTime("aName2",1);
EquationValDrv k0t0 = 
    k0.minus(km1.pow(.2)).
augSys(an20.minus(2)).
    augSys(an10.minus(3));
return(k0t0);
    }
    public	double [] getParams(){return (new double[0]); };
}
