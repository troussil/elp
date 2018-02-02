import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import javax.swing.*;  

/*
 * Moins listener
 */
public class MoinsListener implements ActionListener {

    private JTextField monEcran; 
    private Processeur monProcesseur; 

    MoinsListener(JTextField ecran, Processeur processeur) {
	monEcran = ecran; 
	monProcesseur = processeur; 
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
	monProcesseur.moins();
	monEcran.setText(monProcesseur.obtenirNombreCourant()); 
    }

}