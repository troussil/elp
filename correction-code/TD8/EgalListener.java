import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import javax.swing.*;  

/*
 * Egal listener
 */
public class EgalListener implements ActionListener {

    private JTextField monEcran; 
    private Processeur monProcesseur; 

    EgalListener(JTextField ecran, Processeur processeur) {
	monEcran = ecran; 
	monProcesseur = processeur; 
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
	monEcran.setText(""+monProcesseur.obtenirResultat()); 
    }

}