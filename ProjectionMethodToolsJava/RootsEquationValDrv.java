package gov.frb.ma.msu.ProjectionTools;


import java.util.Arrays;

import org.apache.commons.math.ConvergenceException;
import org.apache.commons.math.FunctionEvaluationException;
import org.apache.commons.math.analysis.solvers.LaguerreSolver;
import org.apache.commons.math.analysis.polynomials.PolynomialFunction;
import org.apache.commons.math.complex.Complex;

import Jama.Matrix;
;
public class RootsEquationValDrv extends EquationValDrv {
	public RootsEquationValDrv(Matrix aVal,Matrix aJac) {
		super(aVal,aJac);
		if(aVal.getColumnDimension()>1)throw new ProjectionRuntimeException("can't have cols>1 for RootsEquationValDrv");
	}
	@SuppressWarnings("deprecation")
	public static RootsEquationValDrv[] applyRootFinderAtPoints(EquationValDrv[] coeffsSource){
		int numNodes=coeffsSource[0].theVal.getRowDimension();
		int numRoots=coeffsSource.length-1;
		Complex[]unSorted =new Complex[coeffsSource.length-1];
		ComplexComparator cmpCmp= new  ComplexComparator();
		int ii,kk;
		double [] coeffsForPoly = new double[coeffsSource.length];
		PolynomialFunction poly;
		LaguerreSolver rFinder;
		Complex[][] allRoots = new Complex[numNodes][numRoots];
		for(ii=0;ii<numNodes;ii++){
			for(kk=0;kk<coeffsSource.length;kk++){
				coeffsForPoly[kk]=	coeffsSource[kk].theVal.get(ii, 0);
			}
			poly=new PolynomialFunction(coeffsForPoly);
			rFinder=new LaguerreSolver();
			try {
				unSorted=rFinder.solveAll(coeffsForPoly, 1.);
				Arrays.sort(unSorted,cmpCmp);
				allRoots[ii]=unSorted;
			} catch (ConvergenceException e) {
				e.printStackTrace();
			} catch (FunctionEvaluationException e) {
				e.printStackTrace();
			}
		}
		RootsEquationValDrv[]	theRes=new RootsEquationValDrv[numRoots];
		Matrix resVal=null;
		Matrix resJac=new Matrix(numNodes,numRoots*numNodes);
		for(kk=0;kk<numRoots;kk++){
			resVal=new Matrix(numNodes,1);
			for(ii=0;ii<numNodes;ii++){
				resVal.set(ii, 0, allRoots[ii][kk].getReal());}
			theRes[kk]=new RootsEquationValDrv(resVal,resJac);}
		return theRes;
	}

	public static RootsEquationValDrv[] applyRootFinderAtPoints(RootEqns theEqns){
double [][] forEval=theEqns.getTheGrid().getTheChebPoly().getXformedOuterProdEvalPoints();
		int numNodes=forEval.length;
		int numRoots=theEqns.getRootEvaler().getNumRoots();
		Complex[]unSorted =new Complex[numRoots];
		ComplexComparator cmpCmp= new  ComplexComparator();
		int ii,kk;
		double [] coeffsForPoly = new double[numRoots+1];
		PolynomialFunction poly;
		LaguerreSolver rFinder;
		Complex[][] allRoots = new Complex[numNodes][numRoots];
		for(ii=0;ii<numNodes;ii++){
		theEqns.setParams(forEval[ii]);
		coeffsForPoly=theEqns.cmpCoeffs();
			poly=new PolynomialFunction(coeffsForPoly);
			rFinder=new LaguerreSolver();
			try {
				unSorted=rFinder.solveAll(coeffsForPoly, 1.);
				Arrays.sort(unSorted,cmpCmp);
				allRoots[ii]=unSorted;
			} catch (ConvergenceException e) {
				e.printStackTrace();
			} catch (FunctionEvaluationException e) {
				e.printStackTrace();
			}
		}
		RootsEquationValDrv[]	theRes=new RootsEquationValDrv[numRoots];
		Matrix resVal=null;
		Matrix resJac=new Matrix(numNodes,numRoots*numNodes);
		for(kk=0;kk<numRoots;kk++){
			resVal=new Matrix(numNodes,1);
			for(ii=0;ii<numNodes;ii++){
				resVal.set(ii, 0, allRoots[ii][kk].getReal());}
			theRes[kk]=new RootsEquationValDrv(resVal,resJac);}
		return theRes;
	}

}
