/**
 * Classe modelisant un etudiant caracterise
 * par son numero (unique), son prenom et son nom
 */
public class Etudiant {

    ///identifiant unique de l'etudiant
    private Integer numero; 
    ///prenom
    private String prenom; 
    ///nom de famille
    private String nom; 

    /**
     * Constructeur
     * @param numero
     * @param prenom
     * @param nom 
     */
    public Etudiant(Integer numero, String prenom, String nom) {
	this.numero = numero; 
	this.prenom = prenom; 
	this.nom = nom; 
    }

    /**
     * Indique si le nom de l'etudiant et un nom donne sont identiques
     * @param unNom un nom donne
     * @return 'True' si les deux noms sont les memes, 'False' sinon
     */
    public boolean aCeNom(String unNom) {
	return (nom.equals(unNom)); 
    }

    /**
     * @return une representation textuelle de l'etudiant 
    */ 
    @Override
    public String toString() {
	return super.toString() + "[" + nom + " " + prenom + ", n° " + numero + "]"; 
    }
}
