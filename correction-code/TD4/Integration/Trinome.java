/**
 * Classe implementant un trinome du second degre
 * ax^2 + bx + c
 */
public class Trinome extends FonctionDeRDansR {

    /** coefficient du monome de puissance 2 */
    private double a; 
    /** coefficient du monome de puissance 1 */
    private double b; 
    /** coefficient du monome de puissance 0 */
    private double c; 
    
    /**
     * Constructeur
     * @param unA
     * @param unB
     * @param unC
     */
    public Trinome(double unA, double unB, double unC) {
	a = unA; 
	b = unB; 
	c = unC; 
    }

    /**
     * Evaluation du trinome en la valeur @a x
     * @param x valeur a laquelle on evalue le trinome
     * @return resultat de l'evaluation, c'est-à-dire
     * ax^2 + bx + c
     */
    @Override
    public double evaluer(double x) {
	return a*x*x + b*x + c; 
    }
}