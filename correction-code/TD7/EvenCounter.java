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
    }

}
