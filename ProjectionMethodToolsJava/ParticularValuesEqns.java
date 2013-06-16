package gov.frb.ma.msu.ProjectionTools;

public class ParticularValuesEqns extends EquateAtGridPoints {
	public ParticularValuesEqns(GridPointsSpec aGrid,int numVals){
		super(aGrid,numVals);

		}
	public void prepEquate(double[][]theVals){

		setValuesToEquate(ParticularValuesEquationValDrv.equateAtPoints(this,theVals));
	
	}
	public double[][] getEvaluationPoints(){
		return(getsState().getXformedChebNodePts());
	}
}
