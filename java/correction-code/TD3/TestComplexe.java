public class TestComplexe {

    public static boolean testsUnitairesMult(Complexe unC) {

	Complexe conjugue = unC.obtenirConjugue();
	System.out.print(unC + " * " + conjugue + " == ");
 
	Complexe resultat1 = conjugue.multiplier(unC);
	System.out.println(resultat1);

	Complexe resultat2 = new Complexe(unC.obtenirNorme(),0);
	return resultat1.comparer(resultat2); 
    }

    public static void main(String[] args) {
	
	boolean res = true; 
	final int n = 5; 
	for (int i = 0; i < n; i++) {
	    for (int j = 0; j < n; j++) {
		Complexe o = new Complexe(i,j); 
		res = res && testsUnitairesMult(o); 
		for (int ii = 0; ii < n; ii++) {
		    for (int jj = 0; jj < n; jj++) {
			res = res && TestVecteur.testsUnitaires(o, new Complexe(ii,jj)); 
		    }
		}
	    }
	}
	String msg = (res) ? "SUCCES" : "ECHEC"; 
	System.out.println(msg); 
    }

}
