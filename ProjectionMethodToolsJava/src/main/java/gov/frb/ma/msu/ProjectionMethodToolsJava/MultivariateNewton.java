package gov.frb.ma.msu.ProjectionMethodToolsJava;

//import optimization.*;
/*************************************************************************
http://www.cs.princeton.edu/introcs/96optimization/
 *  Compilation:  javac MultivariateNewton.java
 *  Execution:    java MultivariateNewton
 *
 *  Run Newton's method to finds the roots and local min/max of
 *  a twice-differentiable function of one variable.
 *
 *************************************************************************/

import Jama.Matrix; 

public class MultivariateNewton {
    public static final double EPSILON = 1e-14;
    public static Matrix delta;
    public static double alpha =1;
    // Newton's method to find x* such that f(x*) = 0, starting at x
    public static Matrix root(Equations f, Matrix x) {
        while (true) {
	    try{
		x=doDelta(f,x);
            if (delta.norm1() < EPSILON) break;
	    } catch (Exception ee) { alpha=alpha/2;x=doDelta(f,x);}
        }
        return x;
    }
    static Matrix doDelta(Equations ff,Matrix xx) {
            Matrix J = ff.jacobian(xx);
            delta = J.inverse().times(ff.eval(xx));
	return(xx.minus(delta));
    }

}

