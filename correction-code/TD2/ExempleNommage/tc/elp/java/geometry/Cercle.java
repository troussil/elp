package tc.elp.java.geometry; 

class Cercle {
    private Point centre; 
    private double rayon; 
    public Cercle(double cx, double cy, double rayon) {
	centre = new Point(cx,cy); 
	this.rayon = java.lang.Math.abs(rayon); 
    }
    public String toString() {
	return "cercle de centre "+centre.toString()+" et rayon "+rayon; 
    }
}
