package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.Integrand;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class AKYShockMod extends DoEqns {

	 AKYShockMod meFunc(){return(this);}
	double [] theShocks = new double[2];


	public double alpha;
	public double anEps;
	public double beta;
	public double chi;
	public double rho;
	public double rhoG;
	public double sigma;
	public double tau;
	public double upsilon;
	 public double get$alpha() { return(alpha);}
	 public double get$anEps() { return(anEps);}
	 public double get$beta() { return(beta);}
	 public double get$chi() { return(chi);}
	 public double get$rho() { return(rho);}
	 public double get$rhoG() { return(rhoG);}
	 public double get$sigma() { return(sigma);}
	 public double get$tau() { return(tau);}
	 public double get$upsilon() { return(upsilon);}
	public void set$alpha(double theVal) { alpha=theVal;}
	public void set$anEps(double theVal) { anEps=theVal;}
	public void set$beta(double theVal) { beta=theVal;}
	public void set$chi(double theVal) { chi=theVal;}
	public void set$rho(double theVal) { rho=theVal;}
	public void set$rhoG(double theVal) { rhoG=theVal;}
	public void set$sigma(double theVal) { sigma=theVal;}
	public void set$tau(double theVal) { tau=theVal;}
	public void set$upsilon(double theVal) { upsilon=theVal;}
	double useShock(final int loc, final StochasticBasis theCollocationSolution){
	if(theCollocationSolution.isPerfectForesightQ()) return(0);else {
	return(theShocks[loc]);}}
	final void doShock(final int loc,final double val){theShocks[loc]=val;}
	public void updateParams(double[] paramVec){
	set$alpha(paramVec[0]);
	set$anEps(paramVec[1]);
	set$beta(paramVec[2]);
	set$chi(paramVec[3]);
	set$rho(paramVec[4]);
	set$rhoG(paramVec[5]);
	set$sigma(paramVec[6]);
	set$tau(paramVec[7]);
	set$upsilon(paramVec[8]);

	}
	 public double[] getParams(){ double [] paramVec = new double[9];paramVec[0]=get$alpha();
	paramVec[1]=get$anEps();
	paramVec[2]=get$beta();
	paramVec[3]=get$chi();
	paramVec[4]=get$rho();
	paramVec[5]=get$rhoG();
	paramVec[6]=get$sigma();
	paramVec[7]=get$tau();
	paramVec[8]=get$upsilon();
	 return(paramVec);
	}

	    public EquationValDrv updateValDrv(StochasticBasis theCollocationSolution) throws ProjectionRuntimeException{
	    	StateVarTime VT;NonStateVarTime VTN;
	VT=new StateVarTime("AA",-1); final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",-1); final EquationValDrv bigDelta$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",-1); final EquationValDrv GG$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("CC",0); final EquationValDrv CC$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("FF",0); final EquationValDrv FF$t=VTN.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("HH",0); final EquationValDrv HH$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("RR",0); final EquationValDrv RR$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("SS",0); final EquationValDrv SS$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=VTN.evalVar(theCollocationSolution);

	VTN=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=VTN.evalVar(theCollocationSolution);


	VT=new StateVarTime("AA$Shock",0); final EquationValDrv AA$Shock$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA$Shock",-1); final EquationValDrv AA$Shock$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG$Shock",0); final EquationValDrv GG$Shock$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG$Shock",-1); final EquationValDrv GG$Shock$tm1=VT.evalVar(theCollocationSolution);

	StateVarTime dVT;
	VTN=new NonStateVarTime("bigPi",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theCollocationSolution,dVT);

	VTN=new NonStateVarTime("FF",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theCollocationSolution,dVT);

	VTN=new NonStateVarTime("SS",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theCollocationSolution,dVT);



	gov.frb.ma.msu.ProjectionTools.GaussHermite gh = theCollocationSolution.getTheGaussHermite();

	Integrand forInt1$Func = new Integrand(){
	public int epsDim(){return(2);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={3, 4};

	doShock(0,0/*epsVec[0]*/);
	doShock(1,0/*epsVec[1]*/);
	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);EquationValDrv forInt1=bigPi$tp1.pow(-1 + anEps).times(FF$tp1.times(-1).times(alpha).times(beta));;
	return(forInt1);}};

	EquationValDrv forInt1=gh.integrate(forInt1$Func,theCollocationSolution);


	Integrand forInt2$Func = new Integrand(){
	public int epsDim(){return(2);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={3, 4};

	doShock(0,0/*epsVec[0]*/);
	doShock(1,0/*epsVec[1]*/);
	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);EquationValDrv forInt2=bigPi$tp1.pow(anEps).times(SS$tp1.times(-1).times(alpha).times(beta));;
	return(forInt2);}};

	EquationValDrv forInt2=gh.integrate(forInt2$Func,theCollocationSolution);


	Integrand forInt3$Func = new Integrand(){
	public int epsDim(){return(2);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={3, 4};

	doShock(0,0/*epsVec[0]*/);
	doShock(1,0/*epsVec[1]*/);
	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);EquationValDrv forInt3=bigPi$tp1.pow(-1 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(SS$tp1).times(varphi3$t).times(-1).times(alpha).times(anEps).times(beta)).plus(bigPi$tp1.pow(-1 + anEps).times(FF$tp1$Drv$bigDelta$t.times(varphi2$t).times(-1).times(alpha).times(beta)).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(alpha).times(beta))).plus(bigPi$tp1.pow(anEps).times(SS$tp1$Drv$bigDelta$t.times(varphi3$t).times(-1).times(alpha).times(beta))).plus(bigPi$tp1.pow(anEps).times(varphi4$tp1.times(alpha).times(beta)))).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(-1).times(alpha).times(anEps).times(beta)));;
	return(forInt3);}};

	EquationValDrv forInt3=gh.integrate(forInt3$Func,theCollocationSolution);


	EquationValDrv eqn1=AA$t.times(HH$t).times(varphi2$t).times(sigma).times(bigDelta$t.pow(-1)).times(CC$t.pow(-1)).plus(CC$t.pow(sigma).times(varphi1$t.times(-1)).plus(1));
	EquationValDrv eqn2=AA$t.times(varphi1$t).times(-1).times(CC$t.pow(sigma)).plus(AA$t.times(varphi2$t)).plus(bigDelta$t.times(upsilon).times(CC$t.pow(sigma)).times(HH$t.pow(chi))).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(1/tau).times(upsilon))).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(chi).times(1/tau).times(upsilon)));
	EquationValDrv eqn3=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(varphi5$t.times(-1)).plus(varphi2$t);
	EquationValDrv eqn4=varphi3$t.plus(varphi5$t);
	EquationValDrv eqn5=bigDelta$tm1.times(bigPi$t).times(varphi4$t).times(-1).times(anEps).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(-1 + anEps)).times(varphi4$t.times(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(-(anEps/(-1 + anEps))).times(FF$t.times(varphi5$t).times(1/(1 - alpha))));
	EquationValDrv eqn6=AA$t.times(HH$t).times(varphi1$t).times(-1).times(bigDelta$t.pow(-2)).plus(AA$t.times(HH$t).times(varphi2$t).times(bigDelta$t.pow(-2)).times(CC$t.pow(-sigma))).plus(bigDelta$t.pow(-2).times(HH$t.pow(1 + chi)).times(varphi3$t.times(1/tau).times(upsilon))).plus(varphi4$t.times(-1).plus(forInt3));
	EquationValDrv eqn7=AA$Shock$tm1.times(-1).plus(AA$t.log()).plus(AA$tm1.log().times(-1).times(rho));
	EquationValDrv eqn8=AA$t.times(HH$t).times(-1).times(bigDelta$t.pow(-1)).plus(CC$t);
	EquationValDrv eqn9=AA$t.times(HH$t).times(-1).times(bigDelta$t.pow(-1)).times(CC$t.pow(-sigma)).plus(FF$t.plus(forInt1));
	EquationValDrv eqn10=bigDelta$t.pow(-1).times(HH$t.pow(1 + chi)).times(-1).times(1/tau).times(upsilon).plus(SS$t.plus(forInt2));
	EquationValDrv eqn11=bigDelta$t.plus(bigDelta$tm1.times(-1).times(alpha).times(bigPi$t.pow(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(-1)).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(alpha));
	EquationValDrv eqn12=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(FF$t.times(-1)).plus(SS$t);
	EquationValDrv eqn13=GG$Shock$tm1.times(-1).plus(GG$t.log()).plus(GG$tm1.log().times(-1).times(rhoG));
	EquationValDrv eqn14=RR$t;
	EquationValDrv eqn15=varphi6$t;

	EquationValDrv sys=eqn1.augSys(eqn2).augSys(eqn3).augSys(eqn4).augSys(eqn5).augSys(eqn6).augSys(eqn7).augSys(eqn8).augSys(eqn9).augSys(eqn10).augSys(eqn11).augSys(eqn12).augSys(eqn13).augSys(eqn14).augSys(eqn15);
	EquationValDrv shockEqn0=AA$Shock$t.minus(theShocks[0]);sys=sys.augSys(shockEqn0);
	EquationValDrv shockEqn1=GG$Shock$t.minus(theShocks[1]);sys=sys.augSys(shockEqn1);
	return(sys);}}