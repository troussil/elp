public class Baigneur implements Runnable {

    private Piscine maPiscine; //piscine (lieu)
    private int monTemps; //temps pour se changer 

    public Baigneur(Piscine unePiscine, int unTemps) {
	maPiscine = unePiscine; 
	monTemps = unTemps; 
    }

    public void run() {
	try {
	    boolean aPrisCabine = false; 
	    synchronized (maPiscine) {
		if (maPiscine.cabineDeLibre()) {
		    maPiscine.prendreCabine();
		    aPrisCabine = true; 
		} else {
		    maPiscine.occuperPieceCollective();
		}
	    }
	    seChanger();
	    if (aPrisCabine) {
		synchronized (maPiscine) {
		    maPiscine.laisserCabine(); 
		}
	    }
	} catch (InterruptedException e) {
	    System.err.println("ERR: interrompu en train de se changer"); 
	}
    }

    public void seChanger() throws InterruptedException {
    	Thread.currentThread().sleep(monTemps); 
    }

}