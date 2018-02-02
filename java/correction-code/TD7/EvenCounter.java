public class EvenCounter implements Runnable {

    private int myCounter; 

    private synchronized int toNextEven() {
	try {
	    myCounter++;
	    Thread.sleep(500); 
	    myCounter++;
	} catch (InterruptedException e) {
	    System.err.println("ERR: Interrupted"); 
	}
	return myCounter; 
    }
    public void run() {
	//premiere solution
	//le bloc "synchronized" permet de coupler le test sur myCounter 
	//et l'appel a toNextEven dans un acces exclusif a "this"
	boolean stop = false; 
	while (!stop) {
	    synchronized(this) {
		if (myCounter < 50) {
		    System.out.println(">" + Thread.currentThread().getName() + " " + toNextEven());
		} else {
		    stop = true; 
		}
	    }
	}
	// //second solution
	// //on appelle toNextEven et on poursuit l'affichage qui 
	// //si la valeur de retour est inferieure au seuil
	// int res = toNextEven(); 
	// while (res <= 50) {
	//     System.out.println(">" + Thread.currentThread().getName() 
	// 		       + ": " + res);
	//     res = toNextEven(); 
	// }
    }

}
