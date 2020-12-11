import java.util.List; 

public class ClientEtudiant2 {

    public static void main(String args[]) {

	//creation du groupe
	GroupeEtudiant groupe = new GroupeEtudiant(); 
	groupe.add( new Etudiant(12, "lskjdf", "qmsldkjf") ); 
	groupe.ajout( new Etudiant(11, "qlmsdjf", "Toto") ); 	
	groupe.ajout( new Etudiant(2, "lsqmkjf ", "mlkjpoi") ); 
	groupe.ajout( new Etudiant(15, "mlqskjdf", "Toto") ); 
	System.out.println(groupe);

	//recherche
	List<Etudiant> etudiants = groupe.recherche("Toto"); 
	for(Etudiant e: etudiants) {
	    System.out.print(e + ", "); 
	}
	System.out.println(); 
    }

}
