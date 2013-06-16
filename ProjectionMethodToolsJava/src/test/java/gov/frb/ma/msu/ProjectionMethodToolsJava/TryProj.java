package gov.frb.ma.msu.ProjectionMethodToolsJava;
import gov.frb.ma.msu.ProjectionMethodToolsJava.DoEqns;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridPointsSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVarSpec;
import gov.frb.ma.msu.ProjectionMethodToolsJava.GridVars;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NewtonSolver;
import gov.frb.ma.msu.ProjectionMethodToolsJava.NonStateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.StateVariablePolynomials;
import gov.frb.ma.msu.ProjectionMethodToolsJava.WeightedStochasticBasis;

import java.io.*;
//import java.util.*;


public class TryProj  {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello World!"); // Display the string.
 InputStreamReader inputStreamReader = new InputStreamReader ( System.in );
    BufferedReader stdin = new BufferedReader ( inputStreamReader );
    boolean hasRequestedQuit = false;
    String line = null;
 //   List result = new ArrayList();
    try {
      while (!hasRequestedQuit) {
        System.out.println("input an integer!\n"); // Display the string.
	System.out.flush();
        line  = stdin.readLine();
        //note that "result" is passed as an "out" parameter
	//        hasRequestedQuit = fInterpreter.parseInput( line, result );
	doit(Integer.parseInt(line,10));
        System.out.println("done!\n"); // Display the string.
      }
    }
    catch ( IOException ex ) {
      System.err.println(ex);
    }
    }

   public static  void doit(int ord) throws Exception
    {
	/* counters for use in for loops*/
	int ii;int jj;

	/*linear and second degree polys*/
	int [] theOrds= new int[1];
	theOrds[0]=ord;
	/*two variables kk and theta non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",0,25);
	GridVarSpec[] svsArray={skk};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= new double[1][ord+1];
	sWts[0][0]=5.2;


	/*create state variable polys*/
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);
sSPoly.setTheWeights(sWts);


	/*create CollocationSolution with one state no non state*/
	//	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(sSPoly,sWts3,snsNames03);

	//System.out.println("here is the start");



	/*two non state variable*/
	double [][]nsWts02= new double[2][ord+1]; 

	/*	    {{5., 0.0004},{5., 0.0004}};*/
	String [] nsNames02={"aName1","aName2"};

	/*create CollocationSolution with one state no non state*/
NonStateVariablePolynomials nSPoly02=new NonStateVariablePolynomials(nsNames02);
sSPoly.setTheWeights(nsWts02);
	WeightedStochasticBasis      spMod=new WeightedStochasticBasis(sSPoly,nSPoly02);


	DoEqns someEqns= new DoMoreElab();

	double [][] allWts = new double [3][ord+1];
	allWts[0][0]=1.2;
	allWts[1][0]=5.2;
	allWts[2][0]=5.2;
	if(ord>0){
	allWts[0][1]=.2;
	allWts[1][1]=.12;
	allWts[2][1]=.02;
	}


	NewtonSolver aSolver=new NewtonSolver();
	
	double [][]	rootRes=aSolver.solveWSB(spMod,allWts,someEqns);
	
	for(ii=0;ii<rootRes.length;ii++){
	    for(jj=0;jj<rootRes[0].length;jj++){
			System.out.println("rootStuff="+rootRes[ii][jj]);
	    }}

    }

}
