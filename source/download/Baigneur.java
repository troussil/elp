/** 
 * Classe modelisant un baigneur qui se met en maillot de bain
 *  dans une piscine. 
 */
public class Baigneur implements Runnable {

    /** lieu de la piscine */
    private Piscine maPiscine; 
    /** temps que le baigneur met pour se changer */
    private int monTemps; 

    /** 
     * Constructeur
     * @param unePiscine lieu de la piscine 
     * @param unTemps temps que le baigneur met pour se changer
     */
    public Baigneur(Piscine unePiscine, int unTemps) {
	maPiscine = unePiscine; 
	monTemps = unTemps; 
    }

    /** 
     * Le baigneur se change soit dans une cabine
     * individuelle, s'il en reste une de libre, 
     * soit dans la piece collective. 
     */
    public void run() {
	try {

	    boolean aPrisCabine = false; 
	    if (maPiscine.cabineDeLibre()) {
		maPiscine.prendreCabine();
		aPrisCabine = true; 
	    } else {
		maPiscine.occuperPieceCollective();
	    }
	    seChanger();
	    if (aPrisCabine) {
		maPiscine.laisserCabine(); 
	    }

	} catch (InterruptedException e) {
	    System.err.println("ERR: interrompu en train de se changer"); 
	}
    }

    /** 
     * Se change pendant une duree egale a monTemps
     */
    public void seChanger() throws InterruptedException {
    	Thread.currentThread().sleep(monTemps); 
    }

}
