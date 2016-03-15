import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import javax.swing.*;  

/*
 * Plus listener
 */
public class PlusListener implements ActionListener {

    private JTextField monEcran; 
    private Processeur monProcesseur; 

    PlusListener(JTextField ecran, Processeur processeur) {
	monEcran = ecran; 
	monProcesseur = processeur; 
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
	monProcesseur.plus();
	monEcran.setText(monProcesseur.obtenirNombreCourant()); 
    }

}