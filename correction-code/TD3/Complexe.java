public class Complexe extends Vecteur {

    public Complexe(double re, double im) {
	super(re,im); 
    }

    public Complexe(Complexe c) {
	super(c.x,c.y); 
    }

    public double obtenirNorme() {
	return x*x + y*y; 
    }

    public Complexe obtenirConjugue() {
	return new Complexe(x, -y); 
    }

    public void multiplier(Complexe c) {
    	double re = x*c.x - y*c.y; 
	double im = x*c.y + y*c.x; 
	x = re; 
	y = im; 
    }

    public void diviser(Complexe c) throws DivisionComplexeParZero {
	double normeDeC = c.obtenirNorme(); 
	if (normeDeC == 0)
	    throw new DivisionComplexeParZero(); 
	else {
	    Complexe conjugueDeC = c.obtenirConjugue(); 
	    multiplier(conjugueDeC); 
	    x /= normeDeC; 
	    y /= normeDeC;
	} 
    }

    public String toString() {
	return x + " + i*" + y; 
    }

}