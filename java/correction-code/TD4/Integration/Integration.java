/** 
* Programme qui integre une fonction (ie. calcule l'aire sous la courbe).
*/
public class Integration
{
    /** 
     * Approximation de l'integration d'une fonction
     * sur un intervalle. 
     * @param a borne inferieure
     * @param b borne superieure
     * @param delta pas d'echantillonage
     * (plus il est petit, meilleure est l'approximation)
     * @param f tout objet implementant l'interface Function
     * @return l'integration de la fonction f
     */
    private static double calculer(double a, double b, double delta, FonctionDeRDansR f)
    {
	System.out.println("integration sur ["+a+","+b+"] par pas de "+delta);
	double sum = 0; 
	for (double x = a; x <= b; x += delta)
	    sum += (f.evaluer(x)*delta); 
	return sum; 
    }

    /** 
     * Approximation de l'integration d'une fonction
     * sur un intervalle. 
     * @param args un tableau d'arguments pour donner
     * les bornes de l'intervalle d'integration et
     * le nombre d'echantillons (plus ce nombre est
     * grand, plus l'estimation est precise). 
     */
    public static void main(String[] args)
    {
	double a, b; //bornes inf et sup de l'intervalle a integrer
	int n; //nombre d'echantillons
	double delta; //pas d'échantillonage = (b-a)/n

	if (args.length >= 3 )
	    {
		a = Double.parseDouble(args[0]); 
		b = Double.parseDouble(args[1]);
		n = Integer.parseInt(args[2]); 
	    }
	else
	    {
		System.out.println("NB: Options <a> <b> <n>"); 
		System.out.println("Pour integrer sur [a,b] avec n échantillons"); 
		a = 0; 
		b = 10; 
		n = 100; 
		System.out.println("NB: Valeurs par défaut: "+a+", "+b+", "+n); 
		System.out.println(); 
	    }

	delta = (b-a)/n;

	//appel principal
	double res, verite; 

	Trinome t = new Trinome(1, 0, 0); 
	res = calculer(a, b, delta, t);
	System.out.println( "Resultat: "+res);
	verite = b*b*b/3.0; //theoreme fondamental du calcul
	System.out.println( "Verite: "+verite);

	Sinusoide s = new Sinusoide(1,0); 
	res = calculer(a,b,delta,s); 
	System.out.println( "Resultat: "+res);
	verite = Math.sin(b); //theoreme fondamental du calcul
	System.out.println( "Verite: "+verite);

    }
}
