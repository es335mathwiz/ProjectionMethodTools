package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class doSingEqnExpSclr extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;
kVT=new StateVarTime("kk",-1);
kVT=new StateVarTime("kk",0);
EquationValDrv k0 = kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",1);
/*
kVT=new StateVarTime("aName1",0);
eqValDrv an10 =kVT.evalVar(theCollocationSolution);
kVT=new StateVarTime("aName1",1);
eqValDrv an1p1 =kVT.evalVar(theCollocationSolution);
*/
EquationValDrv k0t0 = k0.exp().minus(1.5);;
return(k0t0);
    }
    public	double [] getParams(){return (new double[0]); };
}
