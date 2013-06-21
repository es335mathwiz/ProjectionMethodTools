package gov.frb.ma.msu.ProjectionTools;
import Jama.Matrix;
public class ProjCodeMatrix {
	protected Jama.Matrix JamaRepresentation;
	protected org.ujmp.core.Matrix UJMPRepresentation;
	//todo elimninate no argument constructor?
	ProjCodeMatrix(){}
	ProjCodeMatrix(Jama.Matrix theJM){
		JamaRepresentation=theJM;
	}
}

