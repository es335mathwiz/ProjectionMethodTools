package gov.frb.ma.msu.ProjectionTools;

import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.SSEvaluater;

import java.lang.Math;
public  class AssetPricingSS  extends SSEvaluater {
public EquationValDrv updateSSGuess(double[] ssGuess){
EquationValDrv ss1AtPt=makeEVD(1,2,ssGuess[0]);
EquationValDrv ss2AtPt=makeEVD(2,2,ssGuess[1]);
EquationValDrv eq1=ss1AtPt.plus(((ss1AtPt.plus(1).times(Math.exp(getParams()[1])).times(ss1AtPt.pow(-1))).log().times((-1+getParams()[4])).plus((((-1*(getParams()[1]*getParams()[4]))*Math.pow(getParams()[3],-1))+getParams()[1]))).exp().times(ss1AtPt.plus(1).times(Math.pow(getParams()[0],getParams()[4])).times(-1)));
EquationValDrv eq2=((ss1AtPt.plus(1).times(Math.exp(getParams()[1])).times(ss1AtPt.pow(-1))).log().times((-1+getParams()[4])).plus((((-1*(getParams()[1]*getParams()[4]))*Math.pow(getParams()[3],-1))+getParams()[2]))).exp().times(ss2AtPt.plus(1).times(Math.pow(getParams()[0],getParams()[4])).times(-1)).plus(ss2AtPt);
EquationValDrv theSys=eq1.augSys(eq2);
 return theSys;
}

public int getNumSSVals(){return 2;}}



