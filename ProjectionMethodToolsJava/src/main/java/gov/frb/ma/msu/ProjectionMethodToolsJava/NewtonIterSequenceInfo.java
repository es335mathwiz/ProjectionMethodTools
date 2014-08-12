package gov.frb.ma.msu.ProjectionMethodToolsJava;

import java.util.ArrayList;
import java.util.List;

public class NewtonIterSequenceInfo extends ArrayList<NewtonIterInfo>{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;


	boolean newtonConvergedQ=false;

	public boolean isNewtonConvergedQ() {
		return newtonConvergedQ;
	}

	public void setNewtonConvergedQ(boolean newtonConvergedQ) {
		this.newtonConvergedQ = newtonConvergedQ;
	}


	
	private double shrinkFactor;
	private double minShrink;
	public double getMinShrink() {
		return minShrink;
	}

	public double getShrinkFactor() {
		return shrinkFactor;	
	}

	public void setShrinkFactor(double shrinkFactor) {
		this.shrinkFactor = shrinkFactor;	
	}

	public double getShrinkWhenDone() {	
		return shrinkWhenDone;
	}

	public void setShrinkWhenDone(double shrinkWhenDone) {
		this.shrinkWhenDone = shrinkWhenDone;
	}

	public double getNewtonMethodEpsilon() {
		return newtonMethodEpsilon;	
	}

	public void setNewtonMethodEpsilon(double newtonMethodEpsilon) {
		this.newtonMethodEpsilon = newtonMethodEpsilon;
	}

	public int getNewtonMethodMaxIterations() {
		return newtonMethodMaxIterations;
	}

	public void setNewtonMethodMaxIterations(int newtonMethodMaxIterations) {
		this.newtonMethodMaxIterations = newtonMethodMaxIterations;	
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public void setMinShrink(double minShrink) {
		this.minShrink = minShrink;
	}
	
	private double shrinkWhenDone;
	private double newtonMethodEpsilon;
	private int newtonMethodMaxIterations;
	public NewtonIterSequenceInfo() {
		super();
	
	}
	
	public NewtonIterSequenceInfo(double sFactor,double mShrink,double nEps, int nMax) {
		super();

		setShrinkFactor(sFactor);
		setMinShrink(mShrink);

		setNewtonMethodEpsilon(nEps);
		setNewtonMethodMaxIterations(nMax);
	}
}
