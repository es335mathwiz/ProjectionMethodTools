package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
import gov.frb.ma.msu.ProjectionMethodToolsJava.VarTime;
 public class eqns03Mod extends DoEqns {
		public 
		void updateParams(double [] paramVec){};

    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	StateVarTime VT;VarTime dVT;NonStateVarTime VTN;
VT=new StateVarTime("xx",-1); EquationValDrv xx$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("xx",0);EquationValDrv xx$t = VT.evalVar(theProjModel);VT=new StateVarTime("xx",1);
VTN=new NonStateVarTime("yy",0);EquationValDrv yy$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("yy",1);
VTN=new NonStateVarTime("zz",0);EquationValDrv zz$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("zz",1);
VTN=new NonStateVarTime("yy",0);dVT=new StateVarTime("xx",-1);
EquationValDrv yy$t$Drv$xx$tm1=VTN.evalDrvVar(theProjModel,dVT);

/*
eqValDrv eqn498=xx$tm1.pow(2).times(-1).plus(xx$tm1.times(-1).plus(xx$t.plus(-1)));
*/
EquationValDrv eqn498=xx$tm1.pow(2).times(-0.0000001).plus(xx$t.plus(-.1));
EquationValDrv eqn499=xx$t.times(-2).plus(yy$t);
EquationValDrv eqn500=yy$t$Drv$xx$tm1.times(-1).plus(zz$t);
EquationValDrv sys=eqn498.augSys(eqn499).augSys(eqn500);
return(sys);}  public	double [] getParams(){return (new double[0]); };}
