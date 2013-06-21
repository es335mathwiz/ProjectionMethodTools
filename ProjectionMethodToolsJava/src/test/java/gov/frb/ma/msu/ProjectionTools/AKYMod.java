package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.DoEqns;
import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.Integrand;
import gov.frb.ma.msu.ProjectionTools.NonStateVarTime;
import gov.frb.ma.msu.ProjectionTools.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionTools.ShockVarTime;
import gov.frb.ma.msu.ProjectionTools.StateVarTime;
import gov.frb.ma.msu.ProjectionTools.StochasticBasis;

public class AKYMod extends DoEqns{

	AKYMod meFunc(){return(this);}
	double [] theShocks = new double[0];

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

	    public EquationValDrv updateValDrv(final StochasticBasis theCollocationSolution) throws ProjectionRuntimeException{
	StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
	VT=new StateVarTime("AA",-1);

	 final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",-1);

	 final EquationValDrv bigDelta$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",-1);

	 final EquationValDrv GG$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("CC",0); final EquationValDrv CC$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",0); final EquationValDrv FF$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("HH",0); final EquationValDrv HH$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("RR",0); final EquationValDrv RR$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",0); final EquationValDrv SS$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=NVT.evalVar(theCollocationSolution);


	StateVarTime dVT;
	NVT=new NonStateVarTime("bigPi",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("FF",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("SS",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);



	gov.frb.ma.msu.ProjectionTools.GaussHermite gh = theCollocationSolution.getTheGaussHermite();

	Integrand forInt1$Func = new Integrand(){
	public int epsDim(){return(0);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={};
	int [] theSVLocs={};
	theCollocationSolution.setShockVarLocs(theSVLocs);
	theCollocationSolution.setTheShockVals(epsVec);

	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);
	StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
	VT=new StateVarTime("AA",-1);

	 final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",-1);

	 final EquationValDrv bigDelta$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",-1);

	 final EquationValDrv GG$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("CC",0); final EquationValDrv CC$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",0); final EquationValDrv FF$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("HH",0); final EquationValDrv HH$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("RR",0); final EquationValDrv RR$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",0); final EquationValDrv SS$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=NVT.evalVar(theCollocationSolution);


	StateVarTime dVT;
	NVT=new NonStateVarTime("bigPi",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("FF",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("SS",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);



	EquationValDrv forInt1=bigPi$tp1.pow(-1 + anEps).times(FF$tp1.times(-1).times(alpha).times(beta));;
	return(forInt1);}};

	EquationValDrv forInt1=gh.integrate(forInt1$Func,theCollocationSolution);


	Integrand forInt2$Func = new Integrand(){
	public int epsDim(){return(0);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={};
	int [] theSVLocs={};
	theCollocationSolution.setShockVarLocs(theSVLocs);
	theCollocationSolution.setTheShockVals(epsVec);

	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);
	StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
	VT=new StateVarTime("AA",-1);

	 final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",-1);

	 final EquationValDrv bigDelta$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",-1);

	 final EquationValDrv GG$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("CC",0); final EquationValDrv CC$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",0); final EquationValDrv FF$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("HH",0); final EquationValDrv HH$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("RR",0); final EquationValDrv RR$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",0); final EquationValDrv SS$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=NVT.evalVar(theCollocationSolution);


	StateVarTime dVT;
	NVT=new NonStateVarTime("bigPi",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("FF",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("SS",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);



	EquationValDrv forInt2=bigPi$tp1.pow(anEps).times(SS$tp1.times(-1).times(alpha).times(beta).times(tau));;
	return(forInt2);}};

	EquationValDrv forInt2=gh.integrate(forInt2$Func,theCollocationSolution);


	Integrand forInt3$Func = new Integrand(){
	public int epsDim(){return(0);};
	public int numVars(){return(0);};
	public int numNodes(){return(0);};
	public void setShocks(StochasticBasis theCollocationSolution,double[]epsVec){
	int[] newLocs={};
	int [] theSVLocs={};
	theCollocationSolution.setShockVarLocs(theSVLocs);
	theCollocationSolution.setTheShockVals(epsVec);

	theCollocationSolution.updateForShocks(meFunc(),newLocs,epsVec);}
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theCollocationSolution)
	throws ProjectionRuntimeException{
	setShocks(theCollocationSolution,epsVec);
	StateVarTime VT;NonStateVarTime NVT;ShockVarTime VTS;
	VT=new StateVarTime("AA",-1);

	 final EquationValDrv AA$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",-1);

	 final EquationValDrv bigDelta$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",-1);

	 final EquationValDrv GG$tm1=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("AA",0); final EquationValDrv AA$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theCollocationSolution);

	VT=new StateVarTime("GG",0); final EquationValDrv GG$t=VT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("CC",0); final EquationValDrv CC$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",0); final EquationValDrv FF$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("HH",0); final EquationValDrv HH$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("RR",0); final EquationValDrv RR$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",0); final EquationValDrv SS$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=NVT.evalVar(theCollocationSolution);

	NVT=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=NVT.evalVar(theCollocationSolution);


	StateVarTime dVT;
	NVT=new NonStateVarTime("bigPi",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("FF",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);

	NVT=new NonStateVarTime("SS",1);dVT=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=NVT.evalDrvVar(theCollocationSolution,dVT);



	EquationValDrv forInt3=bigPi$tp1.pow(-1 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(SS$tp1).times(varphi3$t).times(-1).times(alpha).times(anEps).times(beta).times(tau)).plus(bigPi$tp1.pow(-1 + anEps).times(FF$tp1$Drv$bigDelta$t.times(varphi2$t).times(-1).times(alpha).times(beta).times(tau)).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(alpha).times(beta).times(tau))).plus(bigPi$tp1.pow(anEps).times(SS$tp1$Drv$bigDelta$t.times(varphi3$t).times(-1).times(alpha).times(beta).times(tau))).plus(bigPi$tp1.pow(anEps).times(varphi4$tp1.times(alpha).times(beta).times(tau)))).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(-1).times(alpha).times(anEps).times(beta).times(tau)));;
	return(forInt3);}};

	EquationValDrv forInt3=gh.integrate(forInt3$Func,theCollocationSolution);


	EquationValDrv eqn1=AA$t.exp().times(bigDelta$t.pow(-1)).times(CC$t.pow(-1)).times(HH$t.times(varphi2$t).times(sigma)).plus(CC$t.pow(sigma).times(varphi1$t.times(-1)).plus(1));
	EquationValDrv eqn2=AA$t.exp().times(CC$t.pow(sigma)).times(varphi1$t.times(-1).times(tau)).plus(AA$t.exp().times(varphi2$t.times(tau)).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(upsilon)))).plus(bigDelta$t.times(tau).times(upsilon).times(CC$t.pow(sigma)).times(HH$t.pow(chi))).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(chi).times(upsilon)));
	EquationValDrv eqn3=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(varphi5$t.times(-1)).plus(varphi2$t);
	EquationValDrv eqn4=varphi3$t.plus(varphi5$t);
	EquationValDrv eqn5=bigDelta$tm1.times(bigPi$t).times(varphi4$t).times(-1).times(anEps).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(-1 + anEps)).times(varphi4$t.times(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(-(anEps/(-1 + anEps))).times(FF$t.times(varphi5$t).times(1/(1 - alpha))));
	EquationValDrv eqn6=AA$t.exp().times(bigDelta$t.pow(-2)).times(CC$t.pow(-sigma)).times(HH$t.times(varphi2$t).times(tau)).plus(AA$t.exp().times(bigDelta$t.pow(-2)).times(HH$t.times(varphi1$t).times(-1).times(tau))).plus(bigDelta$t.pow(-2).times(HH$t.pow(1 + chi)).times(varphi3$t.times(upsilon)).plus(varphi4$t.times(-1).times(tau).plus(forInt3)));
	EquationValDrv eqn7=AA$t.plus(AA$tm1.times(-1).times(rho));
	EquationValDrv eqn8=AA$t.exp().times(bigDelta$t.pow(-1)).times(HH$t.times(-1)).plus(CC$t);
	EquationValDrv eqn9=AA$t.exp().times(bigDelta$t.pow(-1)).times(CC$t.pow(-sigma)).times(HH$t.times(-1)).plus(FF$t.plus(forInt1));
	EquationValDrv eqn10=bigDelta$t.pow(-1).times(HH$t.pow(1 + chi)).times(-1).times(upsilon).plus(SS$t.times(tau).plus(forInt2));
	EquationValDrv eqn11=bigDelta$t.plus(bigDelta$tm1.times(-1).times(alpha).times(bigPi$t.pow(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(-1)).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(alpha));
	EquationValDrv eqn12=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(FF$t.times(-1)).plus(SS$t);
	EquationValDrv eqn13=GG$t.plus(GG$tm1.times(-1).times(rhoG));
	EquationValDrv eqn14=RR$t;
	EquationValDrv eqn15=varphi6$t;

	EquationValDrv sys=eqn1.augSys(eqn2).augSys(eqn3).augSys(eqn4).augSys(eqn5).augSys(eqn6).augSys(eqn7).augSys(eqn8).augSys(eqn9).augSys(eqn10).augSys(eqn11).augSys(eqn12).augSys(eqn13).augSys(eqn14).augSys(eqn15);
	return(sys);}}

