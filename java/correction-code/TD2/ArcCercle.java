class ArcCercle {
    private Cercle cercle; 
    private Secteur secteur; 
    public ArcCercle( double cx, double cy, double r, double theta1, double theta2 ) {
	cercle = new Cercle (cx, cy, r); 
	secteur = new Secteur (theta1, theta2); 
    }
    public String toString() {
	return "arc de "+cercle.toString()+" sur "+secteur.toString(); 
    }
}