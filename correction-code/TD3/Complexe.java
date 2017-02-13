class Complexe extends Vecteur {

    public Complexe(double re, double im) {
	super(re, im); 
    }

    public Complexe(double reelPure) {
	super(reelPure, 0.0); 
    }

    public Complexe(Vecteur v) {
	super(v.x, v.y); 
    }

    public double obtenirNorme() {
	return x * x + y * y; 
    }

    public Complexe obtenirConjugue() {
	return new Complexe(x, -y); 
    }

    public Complexe ajouter(Vecteur v) {
	return new Complexe( super.ajouter(v) ); 
    }

    public Complexe retirer(Vecteur v) {
	return new Complexe( super.retirer(v) ); 
    }

    public Complexe multiplier(Complexe c) {
    	double re = x * c.x - y * c.y; 
	double im = x * c.y + y * c.x; 
	return new Complexe(re, im);
    }

    public Complexe multiplier(double reelPure) {
	return new Complexe(x*reelPure, y*reelPure);
    }

    public String toString() {
	return "(" + x + " + i*" + y + ")"; 
    }

}
