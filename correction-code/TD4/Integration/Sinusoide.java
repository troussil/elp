/**
 * Classe implementant une sinusoide
 * Acos(x)+Bsin(x)
 */
public class Sinusoide extends FonctionDeRDansR {

    /** coefficient du cos */
    private double A; 
    /** coefficient du sin */
    private double B; 
    
    /**
     * Constructeur
     * @param unA
     * @param unB
     */
    public Sinusoide(double unA, double unB) {
	A = unA; 
	B = unB; 
    }

    /**
     * Evaluation de la sinusoide en la valeur @a x
     * @param x valeur a laquelle on evalue 
     * @return resultat de l'evaluation, c'est-à-dire
     * Acos(x)+Bsin(x)
     */
    @Override
    public double evaluer(double x) {
	return A*Math.cos(x) + B*Math.sin(x);  
    }
}