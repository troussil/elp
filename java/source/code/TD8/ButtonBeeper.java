import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 

/*
 * Button listener
 */
public class ButtonBeeper implements ActionListener {

    @Override 
    public void actionPerformed(ActionEvent e) {
	//print 'beep' to the standard output
	//when an action event is received
	System.out.println("beep"); 
    }

}
