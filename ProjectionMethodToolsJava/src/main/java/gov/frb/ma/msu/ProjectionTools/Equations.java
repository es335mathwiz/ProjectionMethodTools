package gov.frb.ma.msu.ProjectionTools;
/*
http://www.cs.princeton.edu/introcs/96optimization/
*/
import Jama.Matrix;

public interface Equations {
    public Matrix eval(Matrix x);           // return N-by-1 function evaluations
    public Matrix jacobian(Matrix x);       // return N-by-1 gradient
}
