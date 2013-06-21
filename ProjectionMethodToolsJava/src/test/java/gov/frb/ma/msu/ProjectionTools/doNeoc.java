package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class doNeoc extends DoEqns {
	public 
	void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	StateVarTime kVT;
kVT=new StateVarTime("kk",-1);
EquationValDrv kk$tm1=kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",0);
EquationValDrv kk$t = kVT.evalVar(theProjModel);
kVT=new StateVarTime("kk",1);
kVT=new StateVarTime("logProd",-1);
EquationValDrv logProd$tm1=kVT.evalVar(theProjModel);
kVT=new StateVarTime("logProd",0);
EquationValDrv logProd$t = kVT.evalVar(theProjModel);
kVT=new StateVarTime("logProd",1);
EquationValDrv logProd$tp1 = kVT.evalVar(theProjModel);
kVT=new StateVarTime("con",0);
EquationValDrv con$t = kVT.evalVar(theProjModel);
kVT=new StateVarTime("con",1);
EquationValDrv con$tp1 = kVT.evalVar(theProjModel);


EquationValDrv     eqn3=kk$t.exp().pow(-0.7).times(logProd$tp1.exp().times(0.3)).plus(0.).times(con$tp1.exp().pow(-2.).times(-0.95)).plus(con$t.exp().pow(-2.));
EquationValDrv     eqn2=kk$t.exp().plus(con$t.exp()).plus(kk$tm1.exp().times(0.)).plus(kk$tm1.exp().pow(0.3).times(logProd$t.exp().times(-1)));
EquationValDrv     eqn1=logProd$t.plus(logProd$tm1.times(-0.3));
    EquationValDrv sys=eqn3.augSys(eqn2).augSys(eqn1);
return(sys);
    }
    public	double [] getParams(){return (new double[0]); };
}
