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
	while (true) {
	    System.out.println(">" + toNextEven());
	}
    }

}