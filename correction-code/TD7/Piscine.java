public class Piscine {
    
    private int monNbCabines = 20; 

    public boolean cabineDeLibre() {
	return (monNbCabines > 0); 
    }

    public void prendreCabine() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans une cabine individuelle" + 
			   " car #cabines=" + monNbCabines); 
	monNbCabines--; 
    }
    
    public void laisserCabine() {
	monNbCabines++; 
    }

    public void occuperPieceCollective() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans la piece collective" + 
			   " car #cabines=" + monNbCabines); 
    }
}