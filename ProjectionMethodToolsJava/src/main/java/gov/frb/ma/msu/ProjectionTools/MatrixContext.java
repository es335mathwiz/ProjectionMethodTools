package gov.frb.ma.msu.ProjectionTools;

//Constructor for Strategy Implementation
public class MatrixContext{
	
	private final MatrixStrategy theStrategy;
	public MatrixContext(MatrixStrategy aStrategy){
		this.theStrategy=aStrategy;		
	}
	
	public ProjCodeMatrix doAdd(ProjCodeMatrix aa, ProjCodeMatrix bb){
		return(theStrategy.add(aa, bb));
	}
	
	public ProjCodeMatrix doSubstract(ProjCodeMatrix aa, ProjCodeMatrix bb){
		return(theStrategy.subtract(aa, bb));
	}
	
	public ProjCodeMatrix doTimes(ProjCodeMatrix aa, ProjCodeMatrix bb){
		return(theStrategy.times(aa, bb));
	}
	
	public ProjCodeMatrix doTimesScalar(ProjCodeMatrix aa, double bb){
		return(theStrategy.timesScalar(aa, bb));
	}
	
	public ProjCodeMatrix doDivide(ProjCodeMatrix aa, ProjCodeMatrix bb){
		return(theStrategy.divide(aa, bb));
	}
	
	public ProjCodeMatrix doExpE(ProjCodeMatrix aa){
		//Element-by-element exponential computation
		return(theStrategy.expE(aa));
	}
	
	public ProjCodeMatrix doPow(ProjCodeMatrix aa, int bb){
		//Matrix power computation
		return(theStrategy.pow(aa, bb));
	}
	
	public ProjCodeMatrix doPowE(ProjCodeMatrix aa, int exponent){
		//Element-by-element power computation
		return(theStrategy.powE(aa, exponent));
	}
	
	public ProjCodeMatrix doLog(ProjCodeMatrix aa){
		return(theStrategy.logE(aa));
	}
	
	public ProjCodeMatrix doFromArray(double[][] array){
		return(theStrategy.fromArray(array));
	}
	
	public double[][] doGetArray(ProjCodeMatrix aa){
		return(theStrategy.getArray(aa));
	}
	
}
