public class TestVecteur {

    public static boolean testsUnitaires(Vecteur unO, Vecteur unV) {

	System.out.print(unO+" + "+unV+" == "); 
	Vecteur v = new Vecteur(unO); 
	v.ajouter(unV); 
	System.out.println(v); 

	System.out.print(v+" - "+unV+" == "); 
	v.retirer(unV); 
	System.out.println(v); 

	return (v.comparer(unO)); 
    }

    public static void main(String[] args) {

	boolean res = true; 
	final int n = 5; 
	for (int i = 0; i < n; i++) {
	    for (int j = 0; j < n; j++) {
		Vecteur o = new Vecteur(i,j); 
		for (int ii = 0; ii < n; ii++) {
		    for (int jj = 0; jj < n; jj++) {
			res = res && testsUnitaires(o, new Vecteur(ii,jj)); 
		    }
		}
	    }
	}
	String msg = (res) ? "SUCCES" : "ECHEC"; 
	System.out.println(msg); 
    }
}