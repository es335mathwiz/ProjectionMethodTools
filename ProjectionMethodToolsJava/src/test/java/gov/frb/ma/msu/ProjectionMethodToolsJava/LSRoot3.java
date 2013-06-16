package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.RootsEquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class LSRoot3 extends DoEqns {

	@Override
	public double[] getParams() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void updateParams(double[] paramVec) {
		// TODO Auto-generated method stub

	}

	@Override
	public EquationValDrv updateValDrv(StochasticBasis theProjModel)
			throws ProjectionRuntimeException {
	StateVarTime VT;
	VT=new StateVarTime("rhoR");EquationValDrv rhoRAtPt=VT.evalVar(theProjModel);
	VT=new StateVarTime("beta");EquationValDrv betaAtPt=VT.evalVar(theProjModel);
	VT=new StateVarTime("gamma2");EquationValDrv gamma2AtPt=VT.evalVar(theProjModel);
	VT=new StateVarTime("tau");EquationValDrv tauAtPt=VT.evalVar(theProjModel);
	VT=new StateVarTime("kappa");EquationValDrv kappaAtPt=VT.evalVar(theProjModel);
	VT=new StateVarTime("rhoR",0);EquationValDrv rhoR=VT.evalVar(theProjModel);
	VT=new StateVarTime("beta",0);EquationValDrv beta=VT.evalVar(theProjModel);
	VT=new StateVarTime("gamma2",0);EquationValDrv gamma2=VT.evalVar(theProjModel);
	VT=new StateVarTime("tau",0);EquationValDrv tau=VT.evalVar(theProjModel);
	VT=new StateVarTime("kappa",0);EquationValDrv kappa=VT.evalVar(theProjModel);
	EquationValDrv[] coeffs={rhoRAtPt.times(-1),betaAtPt.times(-1),gamma2AtPt.times(-1),
rhoRAtPt.plus(1).plus(rhoRAtPt.times(betaAtPt).plus(gamma2AtPt.times(tauAtPt).plus(kappaAtPt.times(tauAtPt))).plus(gamma2AtPt.times(rhoRAtPt).times(tauAtPt).times(-1))),
betaAtPt.times(gamma2AtPt.times(rhoRAtPt.times(tauAtPt))).plus(kappaAtPt.times(tauAtPt.times(-1))).plus(betaAtPt.times(gamma2AtPt.times(tauAtPt).times(-1))).plus(
		betaAtPt.times(rhoRAtPt).times(-1)).plus(betaAtPt.times(-1)).plus(-1),	betaAtPt};
	RootsEquationValDrv root0 =RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[0];
	RootsEquationValDrv root1 =RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[1];
	RootsEquationValDrv root2 =RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[2];

	RootsEquationValDrv root3 =RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[3];

	RootsEquationValDrv root4 =RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[4];
	EquationValDrv eqn00 = root0.times(-1).plus(rhoR);

	EquationValDrv eqn01 = root1.times(-1).plus(beta);

	EquationValDrv eqn02 = root2.times(-1).plus(gamma2);

	EquationValDrv eqn03 = root0.times(-1).plus(tau);

	EquationValDrv eqn04 = root0.times(-1).plus(kappa);


	EquationValDrv sys=eqn00.augSys(eqn01).augSys(eqn02).augSys(eqn03).augSys(eqn04);
	return(sys);
	}

}
