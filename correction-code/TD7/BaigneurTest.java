public class BaigneurTest {

    public static void main(String[] args) {
	Piscine piscine = new Piscine(); 

	int n = 150; //nombre de Baigneur 
	Thread[] baigneurs = new Thread[n]; 
	for (int i = 0; i < n; i++)
	    baigneurs[i] = new Thread( new Baigneur(piscine, 5) ); 
	for (int i = 0; i < n; i++)
	    baigneurs[i].start(); 
    }
}