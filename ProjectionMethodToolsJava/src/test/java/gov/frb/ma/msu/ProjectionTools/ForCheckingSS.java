package gov.frb.ma.msu.ProjectionTools;




import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
import gov.frb.ma.msu.ProjectionTools.SSEvaluater;

import Jama.Matrix;
public class ForCheckingSS extends SSEvaluater {





public EquationValDrv updateSSGuess(double[] ssGuess)
{
EquationValDrv ss1AtPt=makeEVD(1,2,ssGuess[0]);
EquationValDrv ss2AtPt=makeEVD(2,2,ssGuess[1]);
EquationValDrv eq01=ss1AtPt.minus(getParams()[0]);
EquationValDrv eq02=ss2AtPt.plus(ss1AtPt).minus(getParams()[1]);
EquationValDrv sys=eq01.augSys(eq02);
return sys;
}


public EquationValDrv makeEVD(int indx,int size,double theVal){
	Matrix theV=new Matrix(1,1,theVal);
	Matrix theJ=new Matrix(1,size);
	theJ.set(0, indx-1, 1);
	return(new EquationValDrv(theV,theJ));
}
public int getNumSSVals(){return 2;}

}
