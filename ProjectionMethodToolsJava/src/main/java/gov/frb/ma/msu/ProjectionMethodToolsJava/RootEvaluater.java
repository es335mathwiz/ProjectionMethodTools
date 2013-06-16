package gov.frb.ma.msu.ProjectionMethodToolsJava;
//import gov.frb.ma.msu.ProjectionTools.EquationValDrv;
//import gov.frb.ma.msu.ProjectionTools.GridPointsSpec;
//import gov.frb.ma.msu.ProjectionTools.GridVars;
//import gov.frb.ma.msu.ProjectionTools.RootEqns;
//import gov.frb.ma.msu.ProjectionTools.StateVariablePolynomials;
//import gov.frb.ma.msu.ProjectionTools.WeightedStochasticBasis;
public abstract class RootEvaluater  extends Evaluater {
public abstract	EquationValDrv[] genCoeffs(Basis polyBasis);
private WeightedStochasticBasis polyBasis;
public RootEqns setPowersGenBasis(GridVars vSpecs,int[] theOrds){
		GridPointsSpec theGS = new GridPointsSpec(vSpecs,theOrds);
		StateVariablePolynomials sPoly=new StateVariablePolynomials(theGS);
		polyBasis=new WeightedStochasticBasis(sPoly);
		polyBasis.setAllWeights(numCoeffs(theOrds));
		RootEqns rEqns = new RootEqns(genCoeffs(polyBasis), theGS);
		return rEqns;}
		public int numCoeffs(int[]theOrds) {
			int ii,theProd=1;
			for(ii=0;ii<theOrds.length;ii++){
				theProd=theProd*(1+theOrds[ii]);
			}
			return theProd;}
		public abstract int getNumRoots();
		public abstract double [] cmpCoeffs();
}
