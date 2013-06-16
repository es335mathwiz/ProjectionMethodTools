package gov.frb.ma.msu.gsmin;

public class ValueDerivative {
public double [][] theValue;
public double [][] theDeriv;
public ValueDerivative(double [][] aVal,double [][] aDrv){
	theValue= new double[1][aVal[0].length];
	int ii;
	theDeriv=new double[aDrv.length][aDrv[0].length];

	System.arraycopy( aVal[0], 0, theValue[0], 0, aVal[0].length);
	for(ii=0;ii<aDrv.length;ii++){

		System.arraycopy(aDrv[ii], 0,theDeriv[ii], 0, aDrv[ii].length);
	}

	
}
}
