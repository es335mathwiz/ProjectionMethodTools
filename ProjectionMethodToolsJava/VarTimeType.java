package gov.frb.ma.msu.ProjectionTools;

import Jama.Matrix;

public abstract class VarTimeType {
abstract int getLagLead();
private final static int LAGGED=-1;
private final static int CURRENT=0;
private final static int FUTURE=1;
private final static int STATE=-1;
private final static int NONSTATE=0;
private final static int VALUEATNODE=2;
private final static int SHOCK=1;
private final static int DRV=1;
private final static int NOTDRV=0;

public static VarTimeType newVarTimeType(
		int aTime,int stateNonStateShock,int diffWRTVarQ){
switch (diffWRTVarQ){
case DRV:
	return newVarTimeTypeDiff(aTime,stateNonStateShock);
case NOTDRV:
	return newVarTimeTypeNoDiff(aTime,stateNonStateShock);
default:
	throw new IllegalArgumentException("varTime: diffWRTVarQ should be DRV or NOTDRV");
}	}

public static VarTimeType newVarTimeTypeDiff(
		int aTime,int stateNonStateShock){
switch (stateNonStateShock){
case STATE:
	return newVarTimeTypeDiffState(aTime);
	case NONSTATE:
	return newVarTimeTypeDiffNonState(aTime);

default:
	throw new IllegalArgumentException("varTime: stateNonStateShock should be STATE or NONSTATE");
}	}


public static VarTimeType newVarTimeTypeDiffNonState(
		int aTime){
switch (aTime){
case LAGGED:
throw new IllegalArgumentException("varTime: lagged nonstate  not allowed");
	case CURRENT:
	return new DiffNonStateCurrentVarTime();
	case FUTURE:
	return new DiffNonStateFutureVarTime();

default:
	throw new IllegalArgumentException("varTime: timeOffset should be LAGGED, CURRENT or FUTURE");
}	}


public static VarTimeType newVarTimeTypeDiffState(
		int aTime){
switch (aTime){
case LAGGED:
	return new DiffStateLaggedVarTime();
	case CURRENT:
	return new DiffStateCurrentVarTime();
	case FUTURE:
	return new DiffStateFutureVarTime();

default:
	throw new IllegalArgumentException("varTime: timeOffset should be LAGGED, CURRENT or FUTURE");
}	}


public static VarTimeType newVarTimeTypeNoDiff(
		int aTime,int stateNonStateShock){
switch (stateNonStateShock){
case STATE:
	return newVarTimeTypeState(aTime);
	case NONSTATE:
	return newVarTimeTypeNonState(aTime);
	case VALUEATNODE:
		return newVarTimeTypeValueAtNode();
	case SHOCK:
		return newVarTimeTypeShock(aTime);

default:
	throw new IllegalArgumentException("varTime: stateNonStateShock should be STATE or NONSTATE");
}	}


public static VarTimeType newVarTimeTypeNonState(
		int aTime){
switch (aTime){
case LAGGED:
	throw new IllegalArgumentException("varTime: lagged nonstate  not allowed");
	case CURRENT:
	return new NonStateCurrentVarTime();
	case FUTURE:
	return new NonStateFutureVarTime();

default:
	throw new IllegalArgumentException("varTime: timeOffset should be LAGGED, CURRENT or FUTURE");
}	}


public static VarTimeType newVarTimeTypeState(
		int aTime){
switch (aTime){
case LAGGED:
	return new StateLaggedVarTime();
	case CURRENT:
	return new StateCurrentVarTime();
	case FUTURE:
	return new StateFutureVarTime();

default:
	throw new IllegalArgumentException("varTime: timeOffset should be LAGGED, CURRENT or FUTURE");
}	}

public static VarTimeType newVarTimeTypeValueAtNode(){
	return new ValueAtNodeVarTime();
}

public static VarTimeType newVarTimeTypeShock(
		int aTime){
switch (aTime){
case LAGGED:
	return new ShockLaggedVarTime();
	case CURRENT:
	return new ShockCurrentVarTime();


default:
	throw new IllegalArgumentException("varTime: for SHOCK timeOffset should be LAGGED or CURRENT ");
}	}

 Matrix rightAugZeros(Matrix theMat,int zeroCols) {
if(zeroCols==0){return(theMat);} else {
double [][] dblMat=theMat.getArray();
int rowDim=dblMat.length;
int colDim=dblMat[0].length;
double [][] resMat= new double [rowDim][colDim+zeroCols];
int ii;int jj;
for(ii=0;ii<rowDim;ii++){
for(jj=0;jj<colDim;jj++){
  resMat[ii][jj]=dblMat[ii][jj];
}}
return(new Matrix(resMat));}}

EquationValDrv doValSwitch( StochasticBasis model, int nonStateOffset, int rightEnd, int varNum, Matrix laggedMat, Matrix nowMat, Matrix nxtMat, Matrix nowDrvMat, Matrix nxtDrvMat){
	 return new EquationValDrv(new Matrix(0,0),new Matrix(0,0));};;


 EquationValDrv doValSwitch( StochasticBasis model,  int varNum){
		if(true)throw new ProjectionRuntimeException("unimplemented doValSwitch");
		return(new EquationValDrv());	
	};
		
		
		
		EquationValDrv doDrvSwitchState(Basis model,Matrix nxtState,
				Matrix jacobianNxtState,String varName){
			if(true)throw new ProjectionRuntimeException("unimplemented doDrvSwitchState");
			return(new EquationValDrv());	}




EquationValDrv doDrvSwitchNonState(Basis model,double[][][] basisDrvs,
		double [][][] nxtDrvs,VarTime theDrvVar,String varName){
	return(doDrvSwitchNonStateCorrect( model, theDrvVar,varName));

	}
EquationValDrv doDrvSwitchStateCorrect(Basis model,VarTime theDrvVar,String varName){
if(true)	throw new ProjectionRuntimeException();
	return(null);}


	 int getNonStateWtDim(int nonStateVarDim, int nodeDim) {
	int nonStateWtDim=nodeDim*nonStateVarDim;
	return nonStateWtDim;
}
		
		EquationValDrv doDrvSwitchNonStateCorrect(Basis model,VarTime theDrvVar,String varName){
if(true)	throw new ProjectionRuntimeException();
			return(null);}
		
 int getStateWtDim(int stateVarDim, int nodeDim) {
	int stateWtDim=nodeDim*stateVarDim;
	return stateWtDim;
}

protected Matrix padZeroesOnRight(Matrix JJ, int fullWtDim) {
	int colDim=JJ.getColumnDimension();
	if(colDim<fullWtDim)JJ=rightAugZeros(JJ,fullWtDim-colDim);
	return JJ;
}

public static int getCURRENT() {
	return CURRENT;
}

public static int getDRV() {
	return DRV;
}

public static int getFUTURE() {
	return FUTURE;
}

public static int getLAGGED() {
	return LAGGED;
}

public static int getNONSTATE() {
	return NONSTATE;
}

public static int getNOTDRV() {
	return NOTDRV;
}

public static int getSHOCK() {
	return SHOCK;
}

public static int getSTATE() {
	return STATE;
}

public static int getValueAtNode() {
	return VALUEATNODE;
}
}
