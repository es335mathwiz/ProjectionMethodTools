package gov.frb.ma.msu.ProjectionTools;

public interface MatrixStrategy {
	
//	private final ProjCodeMatrix aa;
//	private final ProjCodeMatrix bb;
//	private final double aa, bb;
	
	ProjCodeMatrix add(ProjCodeMatrix aa, ProjCodeMatrix bb);
	ProjCodeMatrix addScalar(ProjCodeMatrix aa, double bb);
	
	ProjCodeMatrix subtract(ProjCodeMatrix aa, ProjCodeMatrix bb);
	ProjCodeMatrix subtractScalar(ProjCodeMatrix aa, double bb);
	
	ProjCodeMatrix times(ProjCodeMatrix aa, ProjCodeMatrix bb);
	ProjCodeMatrix timesE(ProjCodeMatrix aa, ProjCodeMatrix bb);
	ProjCodeMatrix timesScalar(ProjCodeMatrix aa, double bb);
	
	ProjCodeMatrix divide(ProjCodeMatrix aa, ProjCodeMatrix bb);

	ProjCodeMatrix expE(ProjCodeMatrix aa);
	
	ProjCodeMatrix pow(ProjCodeMatrix aa, int exponent);
	ProjCodeMatrix powE(ProjCodeMatrix aa, int exponent);
	
	ProjCodeMatrix logE(ProjCodeMatrix aa);
	
	ProjCodeMatrix fromArray(double[][] array);
	double[][] getArray(ProjCodeMatrix aa);

}