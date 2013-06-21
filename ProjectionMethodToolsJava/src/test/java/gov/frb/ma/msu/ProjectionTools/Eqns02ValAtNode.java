package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class Eqns02ValAtNode extends DoEqns {
			public 
			void updateParams(double [] paramVec){};
	    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	    	StateVarTime VT;NonStateVarTime VTN;
	VT=new StateVarTime("xx"); EquationValDrv xxAtPoint=VT.evalVar(theProjModel);
VT=new StateVarTime("xx",0); EquationValDrv xx$t=VT.evalVar(theProjModel);
	VTN=new NonStateVarTime("yy",0);EquationValDrv yy$t = VTN.evalVar(theProjModel);


EquationValDrv[] coeffs={xxAtPoint,xxAtPoint.pow(2),xxAtPoint.pow(-1),xxAtPoint.pow(-2),xxAtPoint.pow(3),xxAtPoint.pow(6)};
//RootsEquationValDrv lookey = RootsEquationValDrv.applyRootFinderAtPointsNext(coeffs)[0];

	EquationValDrv eqn501=xxAtPoint.pow(2).plus(xx$t.times(-8));
	EquationValDrv eqn502=xxAtPoint.times(-2).plus(yy$t);


	EquationValDrv sys=eqn501.augSys(eqn502);
	return(sys);}
	    public	double [] getParams(){return (new double[0]); };}


