package gov.frb.ma.msu.ProjectionTools;


public class ShockVarTime extends StateVarTime {

	public ShockVarTime(String aName,int aTime){
		super(aName,aTime);
	}	
	public void setVarTime(int aTime){

		setEvaluationTimeAndType(
				VarTimeType.newVarTimeType(aTime,
						VarTimeType.getSHOCK(),VarTimeType.getNOTDRV()));

	}
	
}
