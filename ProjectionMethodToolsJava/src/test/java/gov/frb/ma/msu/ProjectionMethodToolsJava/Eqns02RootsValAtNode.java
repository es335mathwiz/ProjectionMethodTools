package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.RootsEquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;

public class Eqns02RootsValAtNode extends DoEqns {
			public 
			void updateParams(double [] paramVec){};
	    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
	    	StateVarTime VT;NonStateVarTime VTN;
	VT=new StateVarTime("xx"); EquationValDrv xxAtPoint=VT.evalVar(theProjModel);
VT=new StateVarTime("xx",0); EquationValDrv xx$t=VT.evalVar(theProjModel);
	VTN=new NonStateVarTime("yy",0);EquationValDrv yy$t = VTN.evalVar(theProjModel);


//eqValDrv[] coeffs={xxAtPoint,xxAtPoint.pow(2),xxAtPoint.pow(-1),xxAtPoint.pow(-2),xxAtPoint.pow(3),xxAtPoint.pow(6)};

EquationValDrv[] coeffs={xxAtPoint.times(-5),xxAtPoint.times(3),xxAtPoint.pow(2)};


RootsEquationValDrv lookey = RootsEquationValDrv.applyRootFinderAtPoints(coeffs)[0];

	EquationValDrv eqn501=xxAtPoint.pow(2).plus(xx$t.times(-8));
	EquationValDrv eqn502=lookey.times(-1).plus(yy$t);


	EquationValDrv sys=eqn501.augSys(eqn502);
	return(sys);}
	    public	double [] getParams(){return (new double[0]); };}


