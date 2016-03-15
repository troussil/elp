/**
 * Classe modelisant le lieu d'une piscine
 */
public class Piscine {
    
    /** nombre de cabines individuelles */
    private int monNbCabines = 20; 

    /**
     * Verifie si une cabine est libre
     * @return 'true' si au moins une cabine est libre
     * 'false' sinon
     */
    public boolean cabineDeLibre() {
	return (monNbCabines > 0); 
    }

    /**
     * Occupation d'une cabine
     */
    public void prendreCabine() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans une cabine individuelle" + 
			   " car #cabines=" + monNbCabines); 
	monNbCabines--; 
    }
    
    /**
     * Liberation d'une cabine
     */
    public void laisserCabine() {
	monNbCabines++; 
    }

    /**
     * Occupation de la piece collective
     */
    public void occuperPieceCollective() {
	System.out.println(">" + Thread.currentThread().getName() + 
			   " se change dans la piece collective" + 
			   " car #cabines=" + monNbCabines); 
    }
}