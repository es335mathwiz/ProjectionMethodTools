package gov.frb.ma.msu.ProjectionMethodToolsJava;
//import org.ujmp.ujmp.complete.0.3.0.ujmp.complete.0.0.0;
import org.ujmp.core.Matrix;
public class ProjCodeMatrix {
	protected Jama.Matrix JamaRepresentation;
	protected  org.ujmp.core.Matrix UJMPRepresentation;
	//todo elimninate no argument constructor?
	ProjCodeMatrix(){}
	ProjCodeMatrix(Jama.Matrix theJM){
		JamaRepresentation=theJM;
	}
}

