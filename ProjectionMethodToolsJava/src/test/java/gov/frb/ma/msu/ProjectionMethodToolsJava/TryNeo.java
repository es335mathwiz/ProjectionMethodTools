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


public class TryNeo  {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello World!"); // Display the string.
 InputStreamReader inputStreamReader = new InputStreamReader ( System.in );
    BufferedReader stdin = new BufferedReader ( inputStreamReader );
    boolean hasRequestedQuit = false;
    String line = null;
    int ord1;int ord2;
//    List result = new ArrayList();
    try {
      while (!hasRequestedQuit) {
        System.out.println("input an integer capital poly degree!\n"); // Display the string.
	System.out.flush();
        line  = stdin.readLine();
	ord1=Integer.parseInt(line,10);
        System.out.println("input an integer logProd poly degree!\n"); // Display the string.
	System.out.flush();
        line  = stdin.readLine();
	ord2=Integer.parseInt(line,10);
        System.out.println("input a double for EPS about ss!\n"); // Display the string.
	System.out.flush();
        line  = stdin.readLine();
	double EPS=Double.parseDouble(line);
        //note that "result" is passed as an "out" parameter
	//        hasRequestedQuit = fInterpreter.parseInput( line, result );
	doit(ord1,ord2,EPS);
        System.out.println("done!\n"); // Display the string.
      }
    }
    catch ( IOException ex ) {
      System.err.println(ex);
    }
    }

    public static  void doit(int ord1,int ord2,double EPS) throws Exception
    {
	/* counters for use in for loops*/
	int ii;int jj;
	/*

{lx1AMAss -> -1.7932372838764095, ly1AMAss -> -0.8734439214510523, x2AMAss -> 0}


Out[6]//InputForm= 
{-1.0416750123520497 + 0.4191092156525547*lx1[-1 + t] + 
  0.3842194379041953*x2[-1 + t] + 1.2807314596806514*eps[1][t], 
 -0.42061044204059045 + 0.252522900054576*lx1[-1 + t] + 
  0.2664300142619641*x2[-1 + t] + 0.8881000475398804*eps[1][t], 
 0.3*x2[-1 + t] + 1.*eps[1][t]}

	 */

	/*linear and second degree polys*/
    //	     double EPS=1.0e-8;
	int [] theOrds= new int[2];
	theOrds[0]=ord1;	theOrds[1]=ord2;
	/*two variables kk and logProd non standard polys*/
	GridVarSpec skk=new GridVarSpec("kk",-1.7932372838764095-EPS,-1.7932372838764095+EPS);
	GridVarSpec stt=new GridVarSpec("logProd",0-EPS,0+EPS);
	GridVarSpec[] svsArray={skk,stt};
	GridVars stheVars=new GridVars(svsArray);


	/*create the grid ( outer product of polys )*/
	GridPointsSpec stheGS=new GridPointsSpec(stheVars,theOrds);


	/*provide initial weights for StateVariablePolynomialss*/
	double [][]sWts= new double[2][(ord1+1)*(ord2+1)];
	/*

{lx1AMAss -> -1.7932372838764095, ly1AMAss -> -0.8734439214510523, x2AMAss -> 0}
	 */

	sWts[0][0]=-1.7932372838764095;


	/*create state variable polys*/
	StateVariablePolynomials sSPoly=new StateVariablePolynomials(stheGS);



	/*create CollocationSolution with one state no non state*/
	//	NonStateVariablePolynomials nSPoly=new NonStateVariablePolynomials(sSPoly,sWts3,snsNames03);

	//System.out.println("here is the start");



	/*two non state variable*/
	double [][]nsWts02= new double[1][(ord1+1)*(ord2+1)]; 
	nsWts02[0][0]=-0.8734439214510523;


	/*	    {{5., 0.0004},{5., 0.0004}};*/
	String [] nsNames02={"con"};

	/*create CollocationSolution with one state no non state*/
NonStateVariablePolynomials nSPoly02=new NonStateVariablePolynomials(nsNames02);
sSPoly.setTheWeights(nsWts02);
	WeightedStochasticBasis      spMod=new WeightedStochasticBasis(sSPoly,nSPoly02);


	DoEqns someEqns= new doNeoc();

	double [][] allWts = new double [3][(ord1+1)*(ord2+1)];
	

	allWts[0][0]=-1.7932372838764095;
	allWts[1][0]=0;


	/*
Out[6]//InputForm= 
{-1.0416750123520497 + 0.4191092156525547*lx1[-1 + t] + 
  0.3842194379041953*x2[-1 + t] + 1.2807314596806514*eps[1][t], 
 -0.42061044204059045 + 0.252522900054576*lx1[-1 + t] + 
  0.2664300142619641*x2[-1 + t] + 0.8881000475398804*eps[1][t], 
 0.3*x2[-1 + t] + 1.*eps[1][t]}
	*/
	allWts[2][0]=-0.8734439214510523;
	if(ord1>0){
	    /*
	    allWts[0][0]=-1.0416750123520497;
	    allWts[1][0]=-0.42061044204059045;
	    allWts[2][0]=0;
	allWts[0][1]=0.4191092156525547;
	allWts[1][1]=0.252522900054576;
	allWts[2][1]=.3;
	allWts[0][2]= 0.3842194379041953;
	allWts[1][2]=0.2664300142619641;
	allWts[2][2]=.0;
	    */
	    allWts[0][0]=-1.7932372838764095;
	    allWts[1][0]=0;
	    allWts[2][0]=-0.8734439214510523;
	allWts[0][1]=EPS*0.4191092156525547;
	allWts[1][1]=EPS*0.252522900054576;
	allWts[2][1]=EPS*.3;
	allWts[0][2]= EPS*0.3842194379041953;
	allWts[1][2]=EPS*0.2664300142619641;
	allWts[2][2]=EPS*.0;

	}


	NewtonSolver aSolver=new NewtonSolver();
	
	double [][]	rootRes=aSolver.solveWSB(spMod,allWts,someEqns);
	
	for(ii=0;ii<rootRes.length;ii++){
	    for(jj=0;jj<rootRes[0].length;jj++){
			System.out.println("rootStuff="+rootRes[ii][jj]);
	    }}

    }

}
