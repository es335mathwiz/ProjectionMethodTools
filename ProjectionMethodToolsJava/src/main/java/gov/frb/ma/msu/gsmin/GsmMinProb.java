package gov.frb.ma.msu.gsmin;


import java.util.HashSet;
import java.util.Set;
import Jama.Matrix;
import java.util.Iterator;
public class GsmMinProb {
public FAndG theFandG;
public double deltaK;
public int epsilonKAlt=0;
public double epsilonK;
public static Matrix anIdentity;
public Matrix xNow;
public Matrix fVal;
public Matrix nxtFVal;
public Matrix nxtGVals;
public Matrix gVals;
public Matrix fDrvVals;
public Matrix gDrvVals;
int xDim;
public Matrix theNxtXVec;
public double sigma=0;
public double gamma=.5;
public double varrho=.5;
public double epsilon=1;
public double mu=.1;
public double muBar=1;
public double xi=.1;
public int tt = 3;
public double forDeltaK=.1;
public double forEpsilonK=.1;
public double theFactor=2;
public double pProdNorm;
public double [] theGradNormList,theDirNormList;
public double theRho,thePsi,theOmega;
public Set<Integer> theIMinus,theJMinus,theIPlus,theJPlus,theUnion;
public Matrix theH,theN,theB,theLilUk,theP,theS,theV,theD;
public double term230;
public double term229;
public Matrix[] theOldDirs= new Matrix[tt];
public double [] theAlphas=new double[tt];
public double [] theBetas=new double[tt];
public Matrix[] theOldGrads = new Matrix[tt];
public GsmMinProb(){
	super();
    }
public GsmMinProb(final Matrix xVec,final FAndG fg){theFandG=fg;
	xNow=xVec;
	xDim=xVec.getRowDimension();
int ii;
for(ii=0;ii<tt;ii++){
	theOldDirs[ii]=new Matrix(xDim,1);
	theGradNormList = new double[tt];
	theDirNormList=new double[tt];
	theOldGrads[ii]=new Matrix(xDim,1);
	}
employNewXVec(xVec);
}

public GsmMinProb(final double[][]xVec,final FAndG fg){theFandG=fg;
	xNow=new Matrix(xVec);
	xDim=xNow.getRowDimension();
int ii;
for(ii=0;ii<tt;ii++){
	theOldDirs[ii]=new Matrix(xDim,1);
	theGradNormList = new double[tt];
	theDirNormList=new double[tt];
	theOldGrads[ii]=new Matrix(xDim,1);
	}
employNewXVec(xNow);
}

public void iterateToConvergence(final double tol){
	while(theRho>tol){
		
	genSearchDir();
	   doLineSearch();
	    updateForStep1();System.out.println("(rho,psi)=(" +theRho+
	    		","+thePsi+")\n");
}}
public void employNewXVec(final Matrix xVec){
final ValueDerivative theFRes=
	theFandG.evaluateF(xVec);
fVal=new Matrix(theFRes.theValue);
fDrvVals=new Matrix(theFRes.theDeriv);
final ValueDerivative theGRes=	
	theFandG.evaluateG(xVec);
gVals=new Matrix(theGRes.theValue);
gDrvVals=new Matrix(theGRes.theDeriv);
	
	thePsi=psi();

	theJMinus=jMinus();
	theJPlus=jPlus();
	epsilonK=modEpsilonK();
	deltaK=modDeltaK();
	theIMinus=iMinus();
	theIPlus=iPlus();
	theUnion=iMinus();
	theUnion.addAll(iPlus());

	theH=bigH();
	theN=bigN();
	theB=bigB();
	theLilUk=lilUk();
	theP=bigP();
	theRho=lilRho();

}

public void genSearchDir(){
	theAlphas=zapZeroNormsAlpha();	theBetas=zapZeroNormsBeta();
	theS=bigS();
	theV=lilV();
	theD=lilD();
}

public void doLineSearch()
{
	term230=mu*java.lang.Math.pow(theRho,(xi+1));
	term229=(fDrvVals.transpose().times(theD).get(0,0)*mu+(muBar*java.lang.Math.pow(theRho,xi)*thePsi));	
	double lambda=1;final boolean lambdaTooBig=true;

	while(lambdaTooBig){
		try{
		theNxtXVec = xNow.plus(theD.times(lambda));
		final ValueDerivative theFRes=
			theFandG.evaluateF(theNxtXVec);
		nxtFVal=new Matrix(theFRes.theValue);
		final ValueDerivative theGRes=	
			theFandG.evaluateG(theNxtXVec);
		nxtGVals=new Matrix(theGRes.theValue);
		
        
		if(makeConsPlus(lambda)&&makeConsMinus()&&chkF(lambda)) {
			break;
		}
		} catch(final Exception ee) {
			/*just shrink*/
		}

lambda=lambda*varrho;
	}
}

public void updateForStep1(){
	int ii;
	xNow=theNxtXVec.copy();
	for(ii=0;ii<tt-1;ii++){
		theOldGrads[tt-1-ii]=theOldGrads[tt-2-ii];
		theOldDirs[tt-1-ii]=theOldDirs[tt-2-ii];
	}
	theOldGrads[0]=fDrvVals;
	theOldDirs[0]=theD;
	for(ii=0;ii<tt;ii++){
		theGradNormList[ii]=theOldGrads[ii].norm2();
		theDirNormList[ii]=theOldDirs[ii].norm2();
	}
	employNewXVec(xNow);
	if(theP.getRowDimension()==0){pProdNorm=fDrvVals.norm2();} else {
	pProdNorm=theP.times(fDrvVals).norm2();}
}


public boolean makeConsPlus(final double lambda){
	boolean t230Q=true;
	int indexNow;
	
	final Iterator<Integer> it = theJPlus.iterator();
	if(theJPlus.isEmpty()){return(t230Q); }else {
	while(it.hasNext()){
		indexNow=it.next();
	if(nxtGVals.get(0,indexNow)-thePsi+lambda*term230>0) {
		t230Q=false;
	}break;
	}
	}
	return(t230Q);
}
public boolean chkF(final double lambda){
	double sigmaK;
	if(thePsi==0){sigmaK=1;} else {sigmaK=sigma;}
	return((sigmaK*nxtFVal.get(0,0)<=sigmaK*(fVal.get(0,0)+lambda*term229)));
}
public boolean makeConsMinus(){
	boolean t230Q=true;
	int indexNow;
	
	final Iterator<Integer> it = theJMinus.iterator();
	if(theJMinus.isEmpty()){return(t230Q); }else {
	while(it.hasNext()){
		indexNow=it.next();
	if(nxtGVals.get(0,indexNow)>0){t230Q=false;break;}
	}
	}
	return(t230Q);
}



public Set<Integer> jPlus(){ int ii;
final Set<Integer> theSet= new HashSet<Integer>();
for(ii=0;ii<gVals.getColumnDimension();ii++){
	if(gVals.get(0,ii)>0) {
		theSet.add(ii);
	}
}

return(theSet);}
public Set<Integer> jMinus(){ int ii;
final Set<Integer> theSet= new HashSet<Integer>();
for(ii=0;ii<gVals.getColumnDimension();ii++){
	if(gVals.get(0,ii)<=0) {
		theSet.add(ii);
	}
}
return(theSet);}

public static double max(final double []tt) {
    double maximum = tt[0];   // start with the first value
    for (int ii=1; ii<tt.length; ii++) {
        if (tt[ii] > maximum) {
            maximum = tt[ii];   // new maximum
        }
    }
    return maximum;
}//end method max

public static double max(final Matrix colVec){
	return(max((colVec).getArray()[0]));
}
public double psi(){
	return(java.lang.Math.max(0,max(gVals)));}

public Set<Integer> iMinus(){int ii;
final Set<Integer> theSet= new HashSet<Integer>();
for(ii=0;ii<gVals.getColumnDimension();ii++){
	if((-deltaK<=gVals.get(0,ii))&&(gVals.get(0,ii)<0)){theSet.add(ii);
}}return(theSet);}

public double modDeltaK(){
double theMaxNow=0;
int indexNow;
final Iterator<Integer> it = theJMinus.iterator();
if(theJMinus.isEmpty()){return(0); }else {
while(it.hasNext()){
	indexNow=it.next();
theMaxNow=java.lang.Math.max(theMaxNow,-gVals.get(0,indexNow));

}
}
return(forEpsilonK +epsilonKAlt*theMaxNow);
}


public double modEpsilonK() {
	double theMaxNow=0;
	int indexNow;
	final Iterator<Integer> it = theJPlus.iterator();
	if(theJPlus.isEmpty()){return(0); }else {
	while(it.hasNext()){
		indexNow=it.next();
	theMaxNow=java.lang.Math.max(theMaxNow,thePsi-gVals.get(0,indexNow));

	}
}
return(epsilon +epsilonKAlt*theMaxNow);
}


public Set<Integer> iPlus(){int ii;
final Set<Integer> theSet= new HashSet<Integer>();

for(ii=0;ii<gVals.getColumnDimension();ii++){
	if((0<=thePsi-gVals.get(0,ii))&&(thePsi-gVals.get(0,ii)<=epsilonK)){theSet.add(ii);
}}return(theSet);}

public Matrix bigH(){
	final int numElems=theUnion.size();
	
	
	final double [][] theHArray= new double [numElems][numElems];
int indexNow;int ii=0;
final Iterator<Integer> it = theUnion.iterator();
if(theUnion.isEmpty()){return(new Matrix(0,0)); }else {
while(it.hasNext()){
indexNow=it.next();
if(theIPlus.contains(indexNow)){theHArray[ii][ii]=thePsi-gVals.get(0,indexNow);} else {theHArray[ii][ii]=-gVals.get(0,indexNow);}
	ii++;
}
return(new Matrix(theHArray));}
}
public Matrix bigN(){
	if(theUnion.isEmpty()) {return(new Matrix(0,0));} else {

	final int[] intArray=new int[theUnion.size()];int ii;
	
	final int rowDim=gDrvVals.getRowDimension();
	final int[] allArray=new int[rowDim];
	for(ii=0;ii<rowDim;ii++){allArray[ii]=ii;}
	
	int indexNow;ii=0;
	final Iterator<Integer> it = theUnion.iterator();
	
	while(it.hasNext()){
	indexNow=it.next();
	intArray[ii]=indexNow;
		ii++;
	}
	
	
	return(gDrvVals.getMatrix(allArray,intArray));}
}

public Matrix bigB(){
	if(theH.getColumnDimension()==0) {return( new Matrix(0,0));} else {
	return((((theN.transpose().times(theN)).plus(theH)).inverse()).times(theN.transpose()));}
}

public Matrix lilUk(){if(theB.getRowDimension()==0){return(new Matrix(0,0));} else{
	return(((theB.times(-1)).times(fDrvVals)).transpose());}
}
public Matrix bigP(){final int numRows=theN.getRowDimension();
final Matrix theIdentity=Matrix.identity(numRows,numRows);
	return(theIdentity.minus((theN.times(theB))));
	}
public double lilOmega(){
	final Matrix  hProd=theLilUk.times(theH);
	double theSum=0;
	final double [][] hProdArray=hProd.getArray(); final double [][] lilUArray=theLilUk.getArray();
	int ii;final int numElems=hProdArray.length;
	for(ii=0;ii<numElems;ii++){
		theSum=theSum+java.lang.Math.max(hProdArray[ii][0],-1.*lilUArray[ii][0]);
	}
	return(theSum);
}
public double lilRho(){
	if(theUnion.isEmpty()) {
		return(gamma*(java.lang.Math.pow(fDrvVals.norm2(),2)));
	} else{
		int ii;final int numElems=theLilUk.getRowDimension();final double [][]uArray=theLilUk.getArray();
		double theUSum=0;
		for(ii=0;ii<numElems;ii++){
			theUSum=theUSum+uArray[ii][0];
		}
		final Matrix pfProd=theP.times(fDrvVals);
		return(gamma*((java.lang.Math.pow(pfProd.norm2(),2)))+theOmega+thePsi)/(1+java.lang.Math.abs(theUSum));
	
}}

public Matrix bigS(){

	final Matrix alphaSum=sumVectorElems(theOldGrads,theAlphas);
	final Matrix betaSum=sumVectorElems(theOldDirs,theBetas);
	if(theUnion.isEmpty()) {
		return(fDrvVals.times(-1.));
	} else {
		return(theP.times((fDrvVals.times(-1.)).plus(alphaSum).plus(betaSum)));
	}
}
public Matrix sumVectorElems(final Matrix[] theVectors,final double[] theWeights){
	final int numRows=theVectors[0].getRowDimension();final int numCols=theVectors[0].getColumnDimension();
Matrix theSum= new Matrix(numRows,numCols);
int ii;final int numElems=theVectors.length;
for(ii=0;ii<numElems;ii++){
	theSum=theSum.plus(theVectors[ii].times(theWeights[ii]));
}
return(theSum);}


public Matrix lilV(){
	final double [][] hAsArray= theH.getArray();final double [][]uAsArray=theLilUk.getArray();
	if(uAsArray.length==0){return(new Matrix(0,0));} else {
	int ii;final int numElems=uAsArray[0].length;final double [][]resArray=new double[numElems][1];
	for(ii=0;ii<numElems;ii++){
		if(uAsArray[0][ii]<0) {
			resArray[ii][0]=-1-theRho;
		} else {
			resArray[ii][0]=hAsArray[ii][ii]-theRho;
		}
	}if(numElems==0){return(new Matrix(0,0));} else {
	return(new Matrix(resArray));}}
}

public Matrix lilD(){
	if(theUnion.isEmpty()) {
		return(theS.times(java.lang.Math.pow(theRho, xi)));
	} else {
		return((theS.plus((theB.transpose().times(theV)))).times(java.lang.Math.pow(theRho, xi)));
	}
}

public double []zapZeroNormsAlpha(){
	int ii;final int numElems=theGradNormList.length;
	final double[]theRes=new double[numElems];
	for(ii=0;ii<numElems;ii++){
		if(theGradNormList[ii] == 0){theRes[ii]=0;} else 
		{theRes[ii]= -(1-gamma)*epsilon/(theFactor*tt)*pProdNorm/theGradNormList[ii];
}
}return(theRes);}

public double []zapZeroNormsBeta(){
	int ii;final int numElems=theGradNormList.length;
	final double[]theRes=new double[numElems];
	for(ii=0;ii<numElems;ii++){
		if(theGradNormList[ii] == 0){theRes[ii]=0;} else 
		{theRes[ii]= (1-gamma)*(1-epsilon)/(theFactor*tt)*pProdNorm/theDirNormList[ii];
}
}return(theRes);}

/*
 * 
 * 


zapZeroNormsAlpha[theAPs_algParams,pProdNorm_?NumberQ,normList_?VectorQ] :=
    With[ {epsilon = get$epsilon[theAPs],gamma = get$gamma[theAPs],theFactor = get$factor[theAPs],theT = get$tt[theAPs]},
        If[ #==0,
            0,
            -(1-gamma)*epsilon/(theFactor*theT)*pProdNorm/#
        ]&/@normList
    ]

zapZeroNormsBeta[theAPs_algParams,pProdNorm_?NumberQ,normList_?VectorQ] :=
    With[ {epsilon = get$epsilon[theAPs],gamma = get$gamma[theAPs],theFactor = get$factor[theAPs],theT = get$tt[theAPs]},
        If[ #==0,
            0,
            (1-gamma)*(1-epsilon)/(theFactor*theT)*pProdNorm/#
        ]&/@normList
    ]


 *
 */
}



