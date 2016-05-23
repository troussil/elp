public class Etudiant {

    private Integer numero; 
    private String prenom; 
    private String nom; 

    Etudiant(Integer numero, String prenom, String nom) {
	this.numero = numero; 
	this.prenom = prenom; 
	this.nom = nom; 
    }

    public boolean aCeNom(String unNom) {
	return (nom.equals(unNom)); 
    }

    @Override
    public boolean equals(Object unAutre) {
	if (unAutre == null)
	    return false; 
	else {
	    Etudiant e = (Etudiant) unAutre; 
	    return (numero.equals(e.numero)); 
	}
    }

    @Override
    public String toString() {
	return super.toString() + "[" + nom + " " + prenom + ", n° " + numero + "]"; 
    }
}
