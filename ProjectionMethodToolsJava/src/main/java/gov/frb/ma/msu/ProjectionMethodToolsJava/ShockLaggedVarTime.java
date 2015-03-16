package gov.frb.ma.msu.ProjectionMethodToolsJava;



public class ShockLaggedVarTime extends StateLaggedVarTime {

public	EquationValDrv doValSwitch(StochasticBasis model,int varNum){


	return(super.doValSwitch(model,varNum));
	}

public	EquationValDrv doValSwitch(StochasticBasis model,double[] evalPt,int varNum){


	return(super.doValSwitch(model,evalPt,varNum));
	}


	
	
	
	
	
	
	
	

}
