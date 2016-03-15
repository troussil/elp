import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import javax.swing.*;  

/*
 * Chiffre listener
 */
public class ChiffreListener implements ActionListener {

    private JTextField monEcran; 
    private Processeur monProcesseur; 

    ChiffreListener(JTextField ecran, Processeur processeur) {
	monEcran = ecran; 
	monProcesseur = processeur; 
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
	JButton chiffreChoisi = (JButton) e.getSource(); 
	monProcesseur.nouveauChiffre(chiffreChoisi.getText()); 
	monEcran.setText(monProcesseur.obtenirNombreCourant()); 
    }

}