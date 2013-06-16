package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
 public class eqns02IntMod extends DoEqns {
		public 
		void updateParams(double [] paramVec){};
    public EquationValDrv updateValDrv(StochasticBasis theProjModel) throws ProjectionRuntimeException{
StateVarTime VT;NonStateVarTime VTN;
VT=new StateVarTime("xx",-1); 
final EquationValDrv xx$tm1=VT.evalVar(theProjModel);
VT=new StateVarTime("xx",0);
final EquationValDrv xx$t = VT.evalVar(theProjModel);
VT=new StateVarTime("xx",1);
VTN=new NonStateVarTime("yy",0);
final EquationValDrv yy$t = VTN.evalVar(theProjModel);
VTN=new NonStateVarTime("yy",1);
VTN=new NonStateVarTime("zz",0);
final EquationValDrv zz$t = VTN.evalVar(theProjModel);
VTN=new NonStateVarTime("zz",1);
VTN=new NonStateVarTime("yy",1);Integrand forInt01 = new Integrand() {
	public int epsDim(){return(1);};
	public int numVars(){return(xx$tm1.theJac.getColumnDimension());};
	public int numNodes(){return(xx$tm1.theJac.getRowDimension());};

	public void setShocks(StochasticBasis pm,double[] eps){};

public EquationValDrv evaluate(double[] anEps, StochasticBasis theProjModel) 
throws ProjectionRuntimeException{
    EquationValDrv eqnOut=new EquationValDrv((anEps[0]+1)*(anEps[0]+1),xx$tm1.theJac.getColumnDimension(),xx$tm1.theJac.getRowDimension());
    return(eqnOut.plus(xx$tm1));}};


		EquationValDrv theShkRes=new EquationValDrv(0,forInt01.numVars(),forInt01.numNodes());

	try{
	GaussHermite gh = new GaussHermite(3);

		theShkRes=gh.integrate(forInt01,theProjModel);


} catch  (ProjectionRuntimeException ee) {
System.out.println("projectionRuntimeException to set theShkRes>?>?>?>?>?>?>?");
	    System.err.println("Caught projectionRuntimeException:" + ee.getMessage());
//	    ee.printStackTrace();
	}

catch  (Exception ee) {
	System.out.println("projectionRuntimeException to set theShkRes>?>?>?>?>?>?>?");
		    System.err.println("Caught projectionRuntimeException:" + ee.getMessage());
//		    ee.printStackTrace();
		}



System.out.println("after projectionRuntimeException before pluls>?>?>?>?>?>?>?");

EquationValDrv eqn501=xx$tm1.times(-.1).plus(xx$t.plus(-1));
EquationValDrv eqn502=xx$t.times(-.2).plus(yy$t).plus(theShkRes.plus(-2));
EquationValDrv eqn503=yy$t.times(-.1).plus(zz$t);

EquationValDrv sys=eqn501.augSys(eqn502).augSys(eqn503);


return(sys);}
    public	double [] getParams(){return (new double[0]); };}
