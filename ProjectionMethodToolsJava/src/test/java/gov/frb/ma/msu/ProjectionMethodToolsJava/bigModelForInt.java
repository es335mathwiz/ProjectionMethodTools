package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
 public class bigModelForInt extends DoEqns {
		public 
		void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("aa",-1); EquationValDrv aa$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("aa",0);EquationValDrv aa$t = VT.evalVar(theProjModel);
VT=new StateVarTime("bigDelta",-1); EquationValDrv bigDelta$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("bigDelta",0);EquationValDrv bigDelta$t = VT.evalVar(theProjModel);VT=new StateVarTime("bigDelta",1);
VT=new StateVarTime("gamma2",-1); EquationValDrv gamma2$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("gamma2",0);EquationValDrv gamma2$t = VT.evalVar(theProjModel);VT=new StateVarTime("gamma2",1);
VT=new StateVarTime("gamma3",-1); EquationValDrv gamma3$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("gamma3",0);EquationValDrv gamma3$t = VT.evalVar(theProjModel);VT=new StateVarTime("gamma3",1);
VT=new StateVarTime("gg",-1); EquationValDrv gg$tm1=VT.evalVar(theProjModel);VT=new StateVarTime("gg",0);EquationValDrv gg$t = VT.evalVar(theProjModel);VT=new StateVarTime("gg",1);
VTN=new NonStateVarTime("bigPi",0);EquationValDrv bigPi$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("bigPi",1);EquationValDrv bigPi$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("cc",0);EquationValDrv cc$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("cc",1);
VTN=new NonStateVarTime("gamma1",0);EquationValDrv gamma1$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("gamma1",1);
VTN=new NonStateVarTime("gamma4",0);EquationValDrv gamma4$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("gamma4",1);EquationValDrv gamma4$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("gamma5",0);EquationValDrv gamma5$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("gamma5",1);
VTN=new NonStateVarTime("hh",0);EquationValDrv hh$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("hh",1);
VTN=new NonStateVarTime("uu",0);EquationValDrv uu$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("uu",1);EquationValDrv uu$tp1 = VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("vv",0);EquationValDrv vv$t = VTN.evalVar(theProjModel);VTN=new NonStateVarTime("vv",1);EquationValDrv vv$tp1 = VTN.evalVar(theProjModel);

EquationValDrv eqn365=aa$t.times(0.9).times(cc$t.pow(-2).times(gamma2$t.times(-1)).plus(gamma1$t)).plus(gamma3$t.times(-1).times(hh$t.pow(2))).times(bigDelta$t.pow(-2).times(hh$t.times(-1.1111111111111112))).plus(bigPi$tp1.pow(21).times(gamma4$tp1.times(0.9405)).plus(gamma4$t.times(-1)));
EquationValDrv eqn366=bigDelta$tm1.times(gamma4$t).times(21).plus(gamma3$tm1.times(vv$t).times(-0.0001)).times(bigPi$t.pow(-0.9999)).plus(bigPi$t.pow(-0.9999).times(-0.95).plus(1).pow(0.00010001000100001711).times(gamma5$t.times(-20.005992961463807)).plus(gamma2$tm1.times(0.9999)).times(uu$t).plus(bigPi$t.pow(-0.9999).times(-0.95).plus(1).pow(-1.000100010001).times(gamma4$t.times(-4.998502208444397e-6))).times(bigPi$t.pow(-1.9999))).times(0.95);
EquationValDrv eqn367=aa$t.times(gamma2$t).times(hh$t).times(2).times(bigDelta$t.pow(-1)).times(cc$t.pow(-3)).plus(cc$t.pow(-2).plus(gamma1$t.times(-1)));
EquationValDrv eqn368=aa$t.times(hh$t).times(bigDelta$t.pow(-1)).plus(cc$t.times(-1).plus(gg$t.times(-1)));
EquationValDrv eqn369=aa$t.times(hh$t).times(-1).times(bigDelta$t.pow(-1)).times(cc$t.pow(-2)).plus(bigPi$tp1.pow(-0.9999).times(uu$tp1.times(-0.9405)).plus(uu$t));
EquationValDrv eqn370=bigDelta$t.pow(-1).times(hh$t.pow(3)).times(-1.1111111111111112).plus(bigPi$tp1.pow(21).times(vv$tp1.times(-0.9405))).plus(vv$t);
EquationValDrv eqn371=bigDelta$tm1.times(0.95).times(bigPi$t.pow(21)).plus(bigDelta$t.times(-1)).plus(bigPi$t.pow(-0.9999).times(-0.95).plus(1).pow(-0.00010001000100010001).times(0.049985022084443956));
EquationValDrv eqn372=bigPi$t.pow(-0.9999).times(-0.95).plus(1).pow(1.000100010001).times(uu$t.times(-20.00599296146383)).plus(vv$t);
EquationValDrv eqn373=aa$t.log().times(-1).plus(aa$tm1.log().times(0.95));
EquationValDrv eqn374=gg$tm1.times(0.96).plus(gg$t.times(-1));
EquationValDrv eqn375=aa$t.times(0.9).times(cc$t.pow(2).times(gamma1$t).plus(gamma2$t.times(-1))).plus(bigDelta$t.times(0.9).plus(gamma3$t.times(3)).times(cc$t.pow(2).times(hh$t.pow(2)).times(-1)));
EquationValDrv eqn376=bigPi$t.pow(-0.9999).times(-0.95).plus(1).pow(1.000100010001).times(gamma5$t.times(-20.00599296146383)).plus(bigPi$t.pow(-0.9999).times(gamma2$tm1.times(-0.95)).plus(gamma2$t));
EquationValDrv eqn377=bigPi$t.pow(21).times(gamma3$tm1.times(-0.95)).plus(gamma3$t.plus(gamma5$t));

EquationValDrv sys=eqn365.augSys(eqn366).augSys(eqn367).augSys(eqn368).augSys(eqn369).augSys(eqn370).augSys(eqn371).augSys(eqn372).augSys(eqn373).augSys(eqn374).augSys(eqn375).augSys(eqn376).augSys(eqn377);



return(sys);

    };

    public	double [] getParams(){return (new double[0]); };

}
