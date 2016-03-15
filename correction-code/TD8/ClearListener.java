import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import javax.swing.*;  

/*
 * Clear listener
 */
public class ClearListener implements ActionListener {

    private JTextField monEcran; 
    private Processeur monProcesseur; 

    ClearListener(JTextField ecran, Processeur processeur) {
	monEcran = ecran; 
	monProcesseur = processeur; 
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
	monProcesseur.effacer(); 
	monEcran.setText(""); 
    }

}