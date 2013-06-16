package gov.frb.ma.msu.ProjectionTools;

import javax.swing.JPanel;
import javax.swing.JProgressBar;

public class ProgressBarPanel extends JPanel {
	JProgressBar pBar;
	public ProgressBarPanel(int minVal,int maxVal){
	pBar= new JProgressBar();
	pBar.setMinimum(minVal);
	pBar.setMaximum(maxVal);
	add(pBar);
	}
	public void updateBar(int newVal){
		pBar.setValue(newVal);
	}
	private static final long serialVersionUID = 1;

}
