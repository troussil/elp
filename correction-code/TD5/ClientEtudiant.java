import java.util.List; 

public class ClientEtudiant {

    public static void main(String args[]) {

	//creation du groupe
	GroupeEtudiantParListe groupe = new GroupeEtudiantParListe(); 
	groupe.ajout( new Etudiant(12, "lskjdf", "qmsldkjf") ); 
	groupe.ajout( new Etudiant(11, "qlmsdjf", "Toto") ); 	
	groupe.ajout( new Etudiant(2, "lsqmkjf ", "mlkjpoi") ); 
	groupe.ajout( new Etudiant(15, "mlqskjdf", "Toto") ); 

	//recherche
	List<Etudiant> etudiants = groupe.recherche("Toto"); 
	for(Etudiant e: etudiants) {
	    System.out.print(e + ", "); 
	}
	System.out.println(); 
    }

}
