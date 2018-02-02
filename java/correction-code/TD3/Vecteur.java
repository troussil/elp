class Vecteur extends Point {
    
    public Vecteur(double unX, double unY) {
	super(unX, unY) ; 
    }
    
    public boolean comparer(Vecteur v) {
	return (x == v.x) && (y == v.y) ;  
    }
    
    public Vecteur ajouter(Vecteur v) {
	return new Vecteur(x + v.x, y + v.y) ;
    }
    
    public Vecteur retirer(Vecteur v) {
	return new Vecteur(x - v.x, y - v.y) ; 
    }
}
