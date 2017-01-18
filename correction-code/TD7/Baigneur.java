public class Baigneur implements Runnable {

    private Piscine maPiscine; //piscine (lieu)
    private int monTemps; //temps pour se changer 

    public Baigneur(Piscine unePiscine, int unTemps) {
	maPiscine = unePiscine; 
	monTemps = unTemps; 
    }

    public void run() {
	maPiscine.occuperPlace(this); 
    }

    public void seChanger() throws InterruptedException {
    	Thread.currentThread().sleep(monTemps); 
    }

}
