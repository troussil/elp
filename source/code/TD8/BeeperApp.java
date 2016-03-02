import javax.swing.*;       
public class BeeperApp implements Runnable {
    @Override 
    public void run() {
        //Create the window.
        JFrame f = new JFrame("Beeper");
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // Add a button with an action listener
	JButton b = new JButton("Click me"); 
	b.addActionListener( new ButtonBeeper() ); 
        f.getContentPane().add(b);
	//Display the window.
        f.pack();
        f.setVisible(true);
    }
    public static void main(String[] args) {
	//Run the application at the correct time in the event queue.
        SwingUtilities.invokeLater( new BeeperApp() );  
    }
}
