package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.Integrand;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ProjectionRuntimeException;
import gov.frb.ma.msu.ProjectionMethodToolsJava.ShockVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVarTime;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StochasticBasis;
import gov.frb.ma.msu.ProjectionMethodToolsJava.VarTime;

public class experBiggerMod extends DoEqns{
	public double [] theShocks=new double[2];

	experBiggerMod meFunc(){return(this);}

	public double alpha;
	public double anEps;
	public double beta;
	public double chi;
	public double rho;
	public double rhoG;
	public double sigma;
	public double tau;
	 public double get$alpha() { return(alpha);}
	 public double get$anEps() { return(anEps);}
	 public double get$beta() { return(beta);}
	 public double get$chi() { return(chi);}
	 public double get$rho() { return(rho);}
	 public double get$rhoG() { return(rhoG);}
	 public double get$sigma() { return(sigma);}
	 public double get$tau() { return(tau);}
	public void set$alpha(final double theVal) { alpha=theVal;}
	public void set$anEps(final double theVal) { anEps=theVal;}
	public void set$beta(final double theVal) { beta=theVal;}
	public void set$chi(final double theVal) { chi=theVal;}
	public void set$rho(final double theVal) { rho=theVal;}
	public void set$rhoG(final double theVal) { rhoG=theVal;}
	public void set$sigma(final double theVal) { sigma=theVal;}
	public void set$tau(final double theVal) { tau=theVal;}
	@Override
	public void updateParams(final double[] paramVec){
	set$alpha(paramVec[0]);
	set$anEps(paramVec[1]);
	set$beta(paramVec[2]);
	set$chi(paramVec[3]);
	set$rho(paramVec[4]);
	set$rhoG(paramVec[5]);
	set$sigma(paramVec[6]);
	set$tau(paramVec[7]);

	}
	 @Override
	public double[] getParams(){ final double [] paramVec = new double[8];paramVec[0]=get$alpha();
	paramVec[1]=get$anEps();
	paramVec[2]=get$beta();
	paramVec[3]=get$chi();
	paramVec[4]=get$rho();
	paramVec[5]=get$rhoG();
	paramVec[6]=get$sigma();
	paramVec[7]=get$tau();
	 return(paramVec);
	}
	double useShock(final int loc,final StochasticBasis theProjModel){
	if(theProjModel.isPerfectForesightQ())	return(0); else {
	return(theShocks[loc]);
	}
	}
	final void doShock(final int loc,final double val){theShocks[loc]=val;
	
}
  
	    @Override
		public EquationValDrv updateValDrv(final StochasticBasis theProjModel) throws ProjectionRuntimeException{
	    	StateVarTime VT;NonStateVarTime  NVT;ShockVarTime  VTS;
	VT=new StateVarTime("aa",-1); final EquationValDrv aa$tm1=VT.evalVar(theProjModel);

	VT=new StateVarTime("bigDelta",-1); final EquationValDrv bigDelta$tm1=VT.evalVar(theProjModel);

	VT=new StateVarTime("gg",-1); final EquationValDrv gg$tm1=VT.evalVar(theProjModel);

	VT=new StateVarTime("aa",0); final EquationValDrv aa$t=VT.evalVar(theProjModel);

	VT=new StateVarTime("bigDelta",0); final EquationValDrv bigDelta$t=VT.evalVar(theProjModel);

	NVT=new NonStateVarTime("bigPi",0); final EquationValDrv bigPi$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("CC",0); final EquationValDrv CC$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("FF",0); final EquationValDrv FF$t=NVT.evalVar(theProjModel);

	VT=new StateVarTime("gg",0); final EquationValDrv gg$t=VT.evalVar(theProjModel);

	NVT=new NonStateVarTime("HH",0); final EquationValDrv HH$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("RR",0); final EquationValDrv RR$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("SS",0); final EquationValDrv SS$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi1",0); final EquationValDrv varphi1$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi2",0); final EquationValDrv varphi2$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi3",0); final EquationValDrv varphi3$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi4",0); final EquationValDrv varphi4$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi5",0); final EquationValDrv varphi5$t=NVT.evalVar(theProjModel);

	NVT=new NonStateVarTime("varphi6",0); final EquationValDrv varphi6$t=NVT.evalVar(theProjModel);


	VTS=new ShockVarTime("aa$Shock",-1); final EquationValDrv aa$Shock$tm1=VTS.evalVar(theProjModel);

	VTS=new ShockVarTime("gg$Shock",-1); final EquationValDrv gg$Shock$tm1=VTS.evalVar(theProjModel);


	final gov.frb.ma.msu.ProjectionMethodToolsJava.GaussHermite gh = theProjModel.getTheGaussHermite();
	final int lclNumVars=theProjModel.numVarsNow();
	final int lclNumNodes=theProjModel.numNodesNow();
	final Integrand forInt1$Func = new Integrand(){
	public int epsDim(){return(1);};
	public int numVars(){return(lclNumVars);};
	public int numNodes(){return(lclNumNodes);};

	 public void setShocks(StochasticBasis theProjModel,double[] epsVec){
	doShock(0,epsVec[0]);int[] newLocs={1};
	int [] theSVLocs={-1, -1, -1, 0,1};
	theProjModel.setShockVarLocs(theSVLocs);

	theProjModel.updateForShocks(meFunc(), newLocs,epsVec);
	 }

	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theProjModel)
	throws ProjectionRuntimeException{

NonStateVarTime VTN;
VTN=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=VTN.evalVar(theProjModel);

VTN=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=VTN.evalVar(theProjModel);


	EquationValDrv forInt1=bigPi$tp1.pow(-1 + anEps).times(FF$tp1.times(-1).times(alpha).times(beta));;
	return(forInt1);}};

	final EquationValDrv forInt1=gh.integrate(forInt1$Func,theProjModel);


	final Integrand forInt2$Func = new Integrand(){
	public int epsDim(){return(1);};
	public int numVars(){return(lclNumVars);};
	public int numNodes(){return(lclNumNodes);};
	//public double [] theShocks=new double[2];
	

	 public void setShocks(StochasticBasis theProjModel,double[] epsVec){
	doShock(0,epsVec[0]);int[] newLocs={1};theProjModel.updateForShocks(meFunc(), newLocs,epsVec);
	 }

	 
	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theProjModel)
	throws ProjectionRuntimeException{

	NonStateVarTime VTN;

	VTN=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=VTN.evalVar(theProjModel);

	VTN=new NonStateVarTime("FF",1);

	VTN=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=VTN.evalVar(theProjModel);

	VTN=new NonStateVarTime("varphi4",1);


		VTN=new NonStateVarTime("bigPi",1);

		VTN=new NonStateVarTime("FF",1);

		VTN=new NonStateVarTime("SS",1);


	
	EquationValDrv forInt2=bigPi$tp1.pow(anEps).times(SS$tp1.times(-1).times(alpha).times(beta));;
	return(forInt2);}};

	final EquationValDrv forInt2=gh.integrate(forInt2$Func,theProjModel);


	final Integrand forInt3$Func = new Integrand(){
	public int epsDim(){return(1);};
	public int numVars(){return(lclNumVars);};
	public int numNodes(){return(lclNumNodes);};
	

	 public void setShocks(StochasticBasis theProjModel,double[] epsVec){
	doShock(0,epsVec[0]);int[] newLocs={1};theProjModel.updateForShocks(meFunc(), newLocs,epsVec);
	 }

	public EquationValDrv evaluate(double[]epsVec,StochasticBasis theProjModel)
	throws ProjectionRuntimeException{

	NonStateVarTime VTN;VarTime dVTN;

	VTN=new NonStateVarTime("bigPi",1); final EquationValDrv bigPi$tp1=VTN.evalVar(theProjModel);

	VTN=new NonStateVarTime("FF",1); final EquationValDrv FF$tp1=VTN.evalVar(theProjModel);

	VTN=new NonStateVarTime("SS",1); final EquationValDrv SS$tp1=VTN.evalVar(theProjModel);

	VTN=new NonStateVarTime("varphi4",1); final EquationValDrv varphi4$tp1=VTN.evalVar(theProjModel);



		VTN=new NonStateVarTime("bigPi",1);dVTN=new StateVarTime("bigDelta",0); final EquationValDrv bigPi$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theProjModel,dVTN);

		VTN=new NonStateVarTime("FF",1);dVTN=new StateVarTime("bigDelta",0); final EquationValDrv FF$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theProjModel,dVTN);

		VTN=new NonStateVarTime("SS",1);dVTN=new StateVarTime("bigDelta",0); final EquationValDrv SS$tp1$Drv$bigDelta$t=VTN.evalDrvVar(theProjModel,dVTN);


	
	
	EquationValDrv forInt3=bigPi$tp1.pow(-1 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(SS$tp1).times(varphi3$t).times(-1).times(alpha).times(anEps).times(beta)).plus(bigPi$tp1.pow(-1 + anEps).times(FF$tp1$Drv$bigDelta$t.times(varphi2$t).times(-1).times(alpha).times(beta)).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(alpha).times(beta))).plus(bigPi$tp1.pow(anEps).times(SS$tp1$Drv$bigDelta$t.times(varphi3$t).times(-1).times(alpha).times(beta))).plus(bigPi$tp1.pow(anEps).times(varphi4$tp1.times(alpha).times(beta)))).plus(bigPi$tp1.pow(-2 + anEps).times(bigPi$tp1$Drv$bigDelta$t.times(FF$tp1).times(varphi2$t).times(-1).times(alpha).times(anEps).times(beta)));;
	return(forInt3);}};

	final EquationValDrv forInt3=gh.integrate(forInt3$Func,theProjModel);


	final EquationValDrv eqn1=aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(bigDelta$t.pow(-1).times(CC$t.pow(-1)).times(HH$t.times(varphi2$t).times(sigma))).plus(CC$t.pow(sigma).times(varphi1$t.times(-1)).plus(1));
	final EquationValDrv eqn2=aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(CC$t.pow(sigma).times(varphi1$t.times(-1))).plus(aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(varphi2$t).plus(bigDelta$t.times(CC$t.pow(sigma)).times(HH$t.pow(chi))).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(1/tau))).plus(CC$t.pow(sigma).times(HH$t.pow(chi)).times(varphi3$t.times(chi).times(1/tau))));
	final EquationValDrv eqn3=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(varphi5$t.times(-1)).plus(varphi2$t);
	final EquationValDrv eqn4=varphi3$t.plus(varphi5$t);
	final EquationValDrv eqn5=bigDelta$tm1.times(bigPi$t).times(varphi4$t).times(-1).times(anEps).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(-1 + anEps)).times(varphi4$t.times(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(-(anEps/(-1 + anEps))).times(FF$t.times(varphi5$t).times(1/(1 - alpha))));
	final EquationValDrv eqn6=aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(bigDelta$t.pow(-2).times(CC$t.pow(-sigma)).times(HH$t.times(varphi2$t))).plus(aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(bigDelta$t.pow(-2).times(HH$t.times(varphi1$t).times(-1)))).plus(bigDelta$t.pow(-2).times(HH$t.pow(1 + chi)).times(varphi3$t.times(1/tau)).plus(varphi4$t.times(-1).plus(forInt3)));
	final EquationValDrv eqn7=aa$Shock$tm1.plus(aa$t).plus(aa$tm1.times(-1).times(rho));
	final EquationValDrv eqn8=aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(bigDelta$t.pow(-1).times(HH$t.times(-1))).plus(CC$t);
	final EquationValDrv eqn9=aa$Shock$tm1.plus(aa$tm1.times(rho)).exp().times(bigDelta$t.pow(-1).times(CC$t.pow(-sigma)).times(HH$t.times(-1))).plus(FF$t.plus(forInt1));
	final EquationValDrv eqn10=bigDelta$t.pow(-1).times(HH$t.pow(1 + chi)).times(-1).times(1/tau).plus(SS$t.plus(forInt2));
	final EquationValDrv eqn11=bigDelta$t.plus(bigDelta$tm1.times(-1).times(alpha).times(bigPi$t.pow(anEps))).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(-1)).plus(bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(anEps/(-1 + anEps)).times(alpha));
	final EquationValDrv eqn12=bigPi$t.pow(-1 + anEps).times(-1).times(alpha).plus(1).times(1/(1 - alpha)).pow(1/(1 - anEps)).times(FF$t.times(-1)).plus(SS$t);
	final EquationValDrv eqn13=gg$Shock$tm1.plus(gg$t).plus(gg$tm1.times(-1).times(rhoG));
	final EquationValDrv eqn14=RR$t;
	final EquationValDrv eqn15=varphi6$t;

	EquationValDrv sys=eqn1.augSys(eqn2).augSys(eqn3).augSys(eqn4).augSys(eqn5).augSys(eqn6).augSys(eqn7).augSys(eqn8).augSys(eqn9).augSys(eqn10).augSys(eqn11).augSys(eqn12).augSys(eqn13).augSys(eqn14).augSys(eqn15);

	
	return(sys);}

		public double getAlpha() {
			return alpha;
		}
		public void setAlpha(double alpha) {
			this.alpha = alpha;
		}
		public double getAnEps() {
			return anEps;
		}
		public void setAnEps(double anEps) {
			this.anEps = anEps;
		}
		public double getBeta() {
			return beta;
		}
		public void setBeta(double beta) {
			this.beta = beta;
		}
		public double getChi() {
			return chi;
		}
		public void setChi(double chi) {
			this.chi = chi;
		}
		public double getRho() {
			return rho;
		}
		public void setRho(double rho) {
			this.rho = rho;
		}
		public double getRhoG() {
			return rhoG;
		}
		public void setRhoG(double rhoG) {
			this.rhoG = rhoG;
		}
		public double getSigma() {
			return sigma;
		}
		public void setSigma(double sigma) {
			this.sigma = sigma;
		}
		public double getTau() {
			return tau;
		}
		public void setTau(double tau) {
			this.tau = tau;
		}
		public double[] getTheShocks() {
			return theShocks;
		}
		public void setTheShocks(double[] theShocks) {
			this.theShocks = theShocks;
		}


}
