import javax.swing.*;  
import java.awt.*;      
public class Calculatrice implements Runnable {
    @Override 
    public void run() {
        //Create the window
        JFrame f = new JFrame("Calculatrice");
	//Set the behavior for when the window is closed
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

	//Processeur
	Processeur processeur = new Processeur(); 

	//Panel construction
	JPanel premierPanneau = new JPanel(new BorderLayout()); 
	JPanel secondPanneau = new JPanel(new GridLayout(0,3)); 

	JTextField ecran = new JTextField(""); 
	premierPanneau.add(ecran, BorderLayout.PAGE_START); 

	JLabel bas = new JLabel("La fameuse calculatrice en Java"); 
	premierPanneau.add(bas, BorderLayout.PAGE_END); 

	JButton clear = new JButton("C");
	clear.addActionListener( new ClearListener(ecran, processeur) ); 
	premierPanneau.add(clear, BorderLayout.LINE_START); 

	JButton egal = new JButton("="); 
	egal.addActionListener( new EgalListener(ecran, processeur) ); 
	premierPanneau.add(egal, BorderLayout.LINE_END); 

	JButton[] chiffres = new JButton[10]; 
	for (int i = 0; i < 10; i++) {
	    chiffres[i] = new JButton(""+i);
	    chiffres[i].addActionListener( new ChiffreListener(ecran, processeur) );
	}
	for (int i = chiffres.length; i > 0; i--) 
	    secondPanneau.add(chiffres[i-1]); 
	
	JButton moins = new JButton("-"); 
	moins.addActionListener( new MoinsListener(ecran, processeur) ); 
	JButton plus = new JButton("+"); 
	plus.addActionListener( new PlusListener(ecran, processeur) ); 
	secondPanneau.add(moins); 
	secondPanneau.add(plus); 

	premierPanneau.add(secondPanneau, BorderLayout.CENTER); 

        //Add the panel
        f.getContentPane().add(premierPanneau);

        //Set the window size from its components size 
        f.pack();
        //By default, the window is not visible; make it visible
        f.setVisible(true);
    }
    public static void main(String[] args) {
	//Run the application at the correct time in the event queue
        SwingUtilities.invokeLater( new Calculatrice() );  
    }
}
