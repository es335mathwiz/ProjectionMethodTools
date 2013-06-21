package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;
 
public class ParticularValuesEquationValDrv extends EquationValDrv {

	public ParticularValuesEquationValDrv(Matrix aVal,Matrix aJac) {
		super(aVal,aJac);
		if(aVal.getColumnDimension()>1)throw new ProjectionRuntimeException("can't have cols>1 for SSEquationValDrv");
	}

	public static ParticularValuesEquationValDrv[] equateAtPoints(ParticularValuesEqns theEqns, double[][] theVals){
		double [][] forEval=theEqns.getsState().getXformedChebNodePts();
		int numNodes=forEval.length;
		int numSS=theVals[0].length;
		int ii,kk;
		double[][] allSS = new double[numNodes][numSS];

		for(ii=0;ii<numNodes;ii++){
			try {
				allSS[ii]=theVals[ii];
			} catch (Exception ee) {
				ee.printStackTrace();
			} 
		}
		ParticularValuesEquationValDrv[]	theRes=new ParticularValuesEquationValDrv[numSS];
		Matrix resVal=null;
		Matrix resJac=new Matrix(numNodes,numSS*numNodes);
		for(kk=0;kk<numSS;kk++){
			resVal=new Matrix(numNodes,1);
			for(ii=0;ii<numNodes;ii++){
				resVal.set(ii, 0, allSS[ii][kk]);}
			theRes[kk]=new ParticularValuesEquationValDrv(resVal,resJac);}
		return theRes;
	}


}
