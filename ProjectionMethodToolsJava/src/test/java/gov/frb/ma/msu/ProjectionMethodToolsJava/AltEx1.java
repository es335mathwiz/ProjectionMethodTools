package gov.frb.ma.msu.ProjectionMethodToolsJava;

import gov.frb.ma.msu.ProjectionMethodToolsJava.EquationValDrv;
import gov.frb.ma.msu.ProjectionMethodToolsJava.SSEvaluater;


public  class AltEx1  extends SSEvaluater {
	public EquationValDrv updateSSGuess(double[] ssGuess){
		EquationValDrv ss1AtPt=makeEVD(1,6,ssGuess[0]);
		EquationValDrv ss2AtPt=makeEVD(2,6,ssGuess[1]);
		EquationValDrv ss3AtPt=makeEVD(3,6,ssGuess[2]);
		EquationValDrv ss4AtPt=makeEVD(4,6,ssGuess[3]);
		EquationValDrv ss5AtPt=makeEVD(5,6,ssGuess[4]);
		EquationValDrv ss6AtPt=makeEVD(6,6,ssGuess[5]);

		/*{{a, b, c, h, k, y}, {alpha, beta, delta, psi, rho, tau, theta}}*/
		EquationValDrv eq1=ss3AtPt.times(getParams()[6]).times(ss4AtPt.pow((1+getParams()[3]))).plus(ss6AtPt.times(-1).times((1+(-1*getParams()[0]))));
		EquationValDrv eq2=(ss2AtPt).exp().times(ss6AtPt.times(getParams()[0])).plus(ss5AtPt.times((1+(-1*getParams()[2])))).times((-1*getParams()[1])).plus(ss5AtPt);
		EquationValDrv eq3=(ss1AtPt).exp().times(ss5AtPt.pow(getParams()[0])).times(-1).times(ss4AtPt.pow((1+(-1*getParams()[0])))).plus(ss6AtPt);
		EquationValDrv eq4=(ss2AtPt).exp().times(-1).times(ss3AtPt.times(-1).plus(ss6AtPt)).plus(ss5AtPt).plus(ss5AtPt.times(-1).times((1+(-1*getParams()[2]))));
		EquationValDrv eq5=ss1AtPt.plus(ss1AtPt.times(getParams()[4]).times(-1)).plus(ss2AtPt.times(getParams()[5]).times(-1));
		EquationValDrv eq6=ss1AtPt.times(getParams()[5]).times(-1).plus(ss2AtPt).plus(ss2AtPt.times(getParams()[4]).times(-1));
		EquationValDrv theSys=eq1.augSys(eq2).augSys(eq3).augSys(eq4).augSys(eq5).augSys(eq6);
		 return theSys;
		}

public int getNumSSVals(){return 6;}}
