class Secteur {

    private final static double borneSuperieure = 2.0*java.lang.Math.PI; 

    private double theta1, theta2; 

    private double recaler(double angle) {
	if (angle > borneSuperieure)
	    return angle % borneSuperieure; 
	else if (angle < 0.0) 
	    return (angle % borneSuperieure) + borneSuperieure; 
	else 
	    return angle; 
    }

    public Secteur(double theta1, double theta2) {
	this.theta1 = recaler(theta1); 
	this.theta2 = recaler(theta2); 
    }

    public static Secteur creerComplet() {
	return new Secteur(0.0,borneSuperieure); 
    }

    public static Secteur creerPremierQuadrant() {
	return new Secteur(0.0,java.lang.Math.PI/2.0); 
    }

    public double ecart() {
	if (theta2 >= theta1)
	    return (theta2 - theta1); 
	else 
	    return theta2 + borneSuperieure - theta1; 
    }

    public String toString() {
	return "["+theta1+":"+theta2+"]"; 
    }

}