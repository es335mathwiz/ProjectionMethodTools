package gov.frb.ma.msu.ProjectionTools;

public class SSEqns extends EquateAtGridPoints {
	private double [] params;
	private SSEvaluater ssEvaler;

	public SSEqns(GridPointsSpec aGrid,SSEvaluater ssev,double[] initSS){
	super(aGrid,ssev.getNumSSVals());
setSsEvaler(ssev);
	setValuesToEquate(SSEquationValDrv.applySSFinderAtPoints(this,initSS));

	}
	public double[] getParams() {
		return params;
	}
	public void setParams(double[] params) {
		this.params = params;
		getSsEvaler().setParams(params);
	}
	public SSEvaluater getSsEvaler() {
		return ssEvaler;
	}
	public void setSsEvaler(SSEvaluater ssEvaler) {
		this.ssEvaler = ssEvaler;
	}
}