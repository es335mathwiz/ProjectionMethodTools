package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.Basis;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.RootEvaluater;

public class ForCheckingRoot extends RootEvaluater {

	@Override
	public double[] cmpCoeffs() {
		// TODO Auto-generated method stub
	double[] theRes={getParams()[0]*(-1),getParams()[1],getParams()[2],getParams()[4]*getParams()[3]};
	return(theRes);
	}
/*
EquationValDrv[] genCoeffs(WeightedStochasticBasis polyB){
	StateVarTime VT;
	VT=new StateVarTime("rhoR");EquationValDrv rhoRAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("beta");EquationValDrv betaAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("gamma2");EquationValDrv gamma2AtPt=VT.evalVar(polyB);
	VT=new StateVarTime("tau");EquationValDrv tauAtPt=VT.evalVar(polyB);
	VT=new StateVarTime("kappa");EquationValDrv kappaAtPt=VT.evalVar(polyB);
	
	EquationValDrv[] coeffs={rhoRAtPt.times(-1),
rhoRAtPt.plus(1).plus(rhoRAtPt.times(betaAtPt).plus(gamma2AtPt.times(tauAtPt).plus(kappaAtPt.times(tauAtPt))).plus(gamma2AtPt.times(rhoRAtPt).times(tauAtPt).times(-1))),
betaAtPt.times(gamma2AtPt.times(rhoRAtPt.times(tauAtPt))).plus(kappaAtPt.times(tauAtPt.times(-1))).plus(betaAtPt.times(gamma2AtPt.times(tauAtPt).times(-1))).plus(
		betaAtPt.times(rhoRAtPt).times(-1)).plus(betaAtPt.times(-1)).plus(-1),	betaAtPt};

return coeffs;}	
	
	
*/	
	@Override
	public EquationValDrv[] genCoeffs(Basis polyBasis) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getNumRoots() {
		// TODO Auto-generated method stub
		return 3;
	}



}
