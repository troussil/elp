public class Piscine {
    
    private int monNbCabines = 20; 

    public boolean cabineDeLibre() {
	return (monNbCabines > 0); 
    }

    private void prendreCabine() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans une cabine individuelle" + 
			   " car #cabines=" + monNbCabines); 
	monNbCabines--; 
    }
    
    private void laisserCabine() {
	monNbCabines++; 
    }

    private void occuperPieceCollective() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans la piece collective" + 
			   " car #cabines=" + monNbCabines); 
    }

    public void occuperPlace(Baigneur unBaigneur) {
	try {
	    boolean aPrisCabine = false; 
	    synchronized (this) {
		if (cabineDeLibre()) {
		    prendreCabine();
		    aPrisCabine = true; 
		} else {
		    occuperPieceCollective();
		}
	    }
	    unBaigneur.seChanger();
	    if (aPrisCabine) {
		synchronized (this) {
		    laisserCabine(); 
		}
	    }
	} catch (InterruptedException e) {
	    System.err.println("ERR: interrompu en train de se changer"); 
	}
    }

}
