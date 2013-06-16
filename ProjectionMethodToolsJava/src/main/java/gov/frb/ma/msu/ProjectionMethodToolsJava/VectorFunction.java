package gov.frb.ma.msu.ProjectionMethodToolsJava;
import gov.frb.ma.msu.gsmin.ValueDerivative;
import Jama.Matrix;
public interface VectorFunction {

    public ValueDerivative evaluate(Matrix dd) throws ProjectionRuntimeException ;
}
