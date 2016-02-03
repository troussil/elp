public class Vecteur extends Point {

    public Vecteur(double x, double y) {
	super(x,y); 
    }

    public Vecteur(Vecteur v) {
	super(v.x,v.y); 
    }

    public boolean comparer(Vecteur v) {
	return ( (x == v.x) && (y == v.y) );  
    }

    public void ajouter(Vecteur v) {
	x += v.x; 
	y += v.y;  
    }

    public void retirer(Vecteur v) {
    	x -= v.x; 
    	y -= v.y;  
    }

}