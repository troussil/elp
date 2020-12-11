import java.util.List; 
import java.util.ArrayList; 

public class GroupeEtudiant extends ArrayList<Etudiant> {

    public void ajout(Etudiant e) {
	this.add(e); 
    }

    public List<Etudiant> recherche(String nom) {
	List<Etudiant> res = new ArrayList<Etudiant>();  
	for (Etudiant e: this) {
	    if (e.aCeNom(nom))
		res.add(e); 
	}
	return res; 
    }

}
