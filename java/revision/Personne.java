import java.util.Collection;
import java.util.ArrayList;

//--------------------------------------------------------------------
interface CritereDeRecherche {
    public boolean accepte(Personne unePersonne); 
}

//--------------------------------------------------------------------
class FiltreParIdentifiant implements CritereDeRecherche {
    public int monIdentifiant; 
    public FiltreParIdentifiant(int identifiant) {
	monIdentifiant = identifiant; 
    }
    @Override
    public boolean accepte(Personne unePersonne) {
	return (unePersonne.identifiant == monIdentifiant); 
    }    
}
//--------------------------------------------------------------------
class FiltreParNom implements CritereDeRecherche {
    public String monNom; 
    public FiltreParNom(String nom) {
	monNom = nom; 
    }
    @Override
    public boolean accepte(Personne unePersonne) {
	return (unePersonne.nom == monNom); 
    }    
}

//--------------------------------------------------------------------
class Personne {
    public int identifiant;
    public String nom, prenom;
    public Personne(int identifiant, String nom, String prenom) {
	this.identifiant = identifiant;
	this.nom = nom;
	this.prenom = prenom;
    }

    @Override
    public String toString() {
	return "Personne("
	    + identifiant + ","
	    + nom + ","
	    + prenom + ")"; 
    }
    
    static ArrayList<Personne> recherche(Collection<Personne> collection,
					 CritereDeRecherche critere) {
	ArrayList<Personne> res = new ArrayList<Personne>();
	for (Personne pers: collection) {
	    if (critere.accepte(pers)) {
		res.add(pers); 
	    }
	}
	return res; 
    }

    public static void main(String[] args) {
	//construction d'une liste de personne
	ArrayList<Personne> lst = new ArrayList<Personne>();
	lst.add(new Personne(1, "tata", "qlsdkjf")); 
	lst.add(new Personne(2, "tete", "qlsdkjf")); 
	lst.add(new Personne(3, "titi", "qlsdkjf")); 
	lst.add(new Personne(4, "toto", "qlsdkjf")); 
	lst.add(new Personne(5, "tutu", "qlsdkjf"));
	lst.add(new Personne(6, "toto", "qlsdkjf2")); 

	//affichage
	System.out.println("Liste initiale");
	System.out.println(lst);
	System.out.println();
	
	//recherche par identifiant
	FiltreParIdentifiant critere1 = new FiltreParIdentifiant(3); 
	System.out.println("identifiant=3");
	System.out.println(recherche(lst, critere1)); 
	System.out.println();

	//recherche par identifiant
	FiltreParNom critere2 = new FiltreParNom("toto"); 
	System.out.println("nom=toto");
	System.out.println(recherche(lst, critere2)); 
	System.out.println();
    }
}
