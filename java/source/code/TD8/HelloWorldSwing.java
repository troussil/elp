import javax.swing.*;       
public class HelloWorldSwing implements Runnable {
    @Override 
    public void run() {
        //Create the window
        JFrame f = new JFrame("HelloWorldSwing");
	//Set the behavior for when the window is closed
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //Add a label
        f.getContentPane().add(new JLabel("Hello world!"));
        //Set the window size from its components size 
        f.pack();
        //By default, the window is not visible; make it visible
        f.setVisible(true);
    }
    public static void main(String[] args) {
	//Run the application at the correct time in the event queue
        SwingUtilities.invokeLater( new HelloWorldSwing() );  
    }
}
