package gov.frb.ma.msu.ProjectionMethodToolsJava;

import Jama.Matrix;

public class SSEquationValDrv extends EquationValDrv {


		public SSEquationValDrv(Matrix aVal,Matrix aJac) {
			super(aVal,aJac);
			if(aVal.getColumnDimension()>1)throw new ProjectionRuntimeException("can't have cols>1 for SSEquationValDrv");
		}

		public static SSEquationValDrv[] applySSFinderAtPoints(SSEqns theEqns, double[] initSS){
			double [][] forEval=theEqns.getsState().getXformedChebNodePts();
			int numNodes=forEval.length;
			int numSS=theEqns.getSsEvaler().getNumSSVals();
			int ii,kk;
			double[][] allSS = new double[numNodes][numSS];
			NewtonSolver solver = new NewtonSolver();
			solver.setNewtonMethodEpsilon(1.0e-8);
//			double[]initSS= new double[numSS];
			for(ii=0;ii<numNodes;ii++){
				try {
					theEqns.setParams(forEval[ii]);
					allSS[ii]=solver.solveSS(theEqns,initSS);
				} catch (Exception ee) {
					ee.printStackTrace();
				} 
			}
			SSEquationValDrv[]	theRes=new SSEquationValDrv[numSS];
			Matrix resVal=null;
			Matrix resJac=new Matrix(numNodes,numSS*numNodes);
			for(kk=0;kk<numSS;kk++){
				resVal=new Matrix(numNodes,1);
				for(ii=0;ii<numNodes;ii++){
					resVal.set(ii, 0, allSS[ii][kk]);}
				theRes[kk]=new SSEquationValDrv(resVal,resJac);}
			return theRes;
		}


	}


