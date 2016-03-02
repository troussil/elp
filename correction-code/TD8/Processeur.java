abstract class Operateur {
    public abstract double faire(double a, double b); 
}

class OperateurPlus extends Operateur {
    public double faire(double a, double b) {
	return (a+b); 
    }     
}

class OperateurMoins extends Operateur {
    public double faire(double a, double b) {
	return (a-b); 
    }     
}

/**
 * Classe calculant incrémentalement une suite
 * d'additions et de soustractions. Les nombres
 * sont saisis chiffre par chiffre (du poids le
 * plus fort au poids le plus faible, comme dans
 * une calculatrice). 
 */
public class Processeur {

    /** Somme temporaire */
    private double maSomme; 
    /** Representation textuelle du nouveau nombre
	a ajouter ou a retrancher de maSomme */
    private String monNombre = ""; 
    /** Operateur indiquant la prochaine operation a effectuer */
    private Operateur operateur; 

    /**
     * Incremente monNombre d'un nouveau chiffre
     * @param chiffre nouveau chiffre
     * (1 caractere de "0" a "9")
     */
    public void nouveauChiffre(String chiffre) {
	monNombre += chiffre; 
    }

    /**
     * Calcule la somme intermediaire si 
     * le dernier operateur est diponsible, 
     * realise une simple affection sinon. 
     */
    public void calculer() {
	if (operateur != null) {
	    maSomme = operateur.faire(maSomme, Double.parseDouble(monNombre));
	    operateur = null; 
	} else {
	    if (!monNombre.equals(""))
		maSomme = Double.parseDouble(monNombre); 
	}
	System.err.println("= " + maSomme); 
	monNombre = ""; 
    }

    /**
     * Calcule la somme intermediaire 
     * et cree un operateur plus
     */
    public void plus() {
	calculer(); 
	operateur = new OperateurPlus(); 
    }

    /**
     * Calcule la somme intermediaire 
     * et cree un operateur moins
     */
    public void moins() {
	calculer(); 
	operateur = new OperateurMoins(); 
    }

    /**
     * Remise a zero
     */
    public void effacer() {
	maSomme = 0.0; 
	monNombre = ""; 
	operateur = null; 
    }

    /**
     * @return nombre courant
     */
    public String obtenirNombreCourant() {
	return monNombre; 
    }

    /**
     * @return resultat
     */
    public double obtenirResultat() {
	calculer(); 
	return maSomme; 
    }

}