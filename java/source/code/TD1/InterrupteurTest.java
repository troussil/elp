/** Classe testant l'interrupteur à bascule. */
class InterrupteurTest {
    public static void main(String[] args) {
	Interrupteur i = new Interrupteur(); 
	//TODO
	int nb = 0, nbok = 0; 
	if ( !i.estEnMarche ) //la lampe est créé éteinte
	    nbok++; 
	nb++; 
	i.basculer(); //on appuie sur l'interrupteur
	if ( i.estEnMarche ) //... et la lampe s'allume 
	    nbok++; 
	nb++; 
	
	String res = (nb == nbok)?"OK":"KO"; 
	System.out.println( res ); 
    }
}