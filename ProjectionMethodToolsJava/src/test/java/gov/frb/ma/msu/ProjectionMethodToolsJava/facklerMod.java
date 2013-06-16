package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
 public class facklerMod extends DoEqns {
 facklerMod meFunc(){return(this);}
double [] theShocks = new double[0];

double useShock(final int loc, final StochasticBasis theProjModel){
if(theProjModel.isPerfectForesightQ()) return(0);else {
return(theShocks[loc]);}}
final void doShock(final int loc,final double val){theShocks[loc]=val;}
public void updateParams(double[] paramVec){

}
 public double[] getParams(){ double [] paramVec = new double[0]; return(paramVec);
}

    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
    	StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("pp",-1); final EquationValDrv pp$tm1=VT.evalVar(theProjModel);

VT=new StateVarTime("pp",0); final EquationValDrv pp$t=VT.evalVar(theProjModel);

VTN=new NonStateVarTime("ss",0); final EquationValDrv ss$t=VTN.evalVar(theProjModel);


//gov.frb.ma.msu.gaussHermite gh = theCollocationSolution.getTheGaussHermite();

EquationValDrv eqn3=pp$t.plus(pp$tm1.times(-1));
EquationValDrv eqn4=pp$tm1.plus(ss$t.pow(0.5).times(-1)).plus(ss$t.pow(2).times(-1)).plus(pp$tm1.pow(2.5).times(ss$t.times(-0.6666666666666666)));

EquationValDrv sys=eqn3.augSys(eqn4);
return(sys);}}


