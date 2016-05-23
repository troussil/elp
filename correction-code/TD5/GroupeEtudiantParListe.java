import java.util.List; 
import java.util.ArrayList; 

public class GroupeEtudiantParListe {

    private List<Etudiant> maListe = new ArrayList<Etudiant>(); 

    public void ajout(Etudiant e) {
	maListe.add(e); 
    }

    public List<Etudiant> recherche(String nom) {
	List<Etudiant> res = new ArrayList<Etudiant>();  
	for (Etudiant e: maListe) {
	    if (e.aCeNom(nom))
		res.add(e); 
	}
	return res; 
    }

}
