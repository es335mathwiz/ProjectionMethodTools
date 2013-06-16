package gov.frb.ma.msu.ProjectionMethodToolsJava;

public class RootEqns extends EquateAtGridPoints {
	private RootEvaluater rootEvaler;
	public RootEqns(EquationValDrv[] coeffs,GridPointsSpec aGrid ){
	super(aGrid,coeffs.length-1);
		setTheCoeffs(coeffs);
		setValuesToEquate(RootsEquationValDrv.applyRootFinderAtPoints(coeffs));

	}	
	public RootEqns(GridPointsSpec aGrid,RootEvaluater rootEv ){
		super(aGrid,rootEv.getNumRoots());
		setTheGrid(aGrid);
		setRootEvaler(rootEv);
		setValuesToEquate(RootsEquationValDrv.applyRootFinderAtPoints(this));

		}
	public RootEvaluater getRootEvaler() {
		return rootEvaler;
	}
	public void setRootEvaler(RootEvaluater rootEvaler) {
		this.rootEvaler = rootEvaler;
	}	
	public void setParams(double[] params) {
		getRootEvaler().setParams(params);
	}
public double [] cmpCoeffs(){
	return(getRootEvaler().cmpCoeffs());
}
}