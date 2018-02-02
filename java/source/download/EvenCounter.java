/**
 * Counter of even numbers
 */
public class EvenCounter implements Runnable {

    /** counter */
    private int myCounter; 

    /**
     * Increments the counter twice
     * in order to return the next even number
     * @return next even
     */
    private int toNextEven() {
	try {
	    myCounter++;
	    Thread.sleep(500); 
	    myCounter++;
	} catch (InterruptedException e) {
	    System.err.println("ERR: Interrupted"); 
	}
	return myCounter; 
    }

    /**
     * Displays to standard output the list of 
     * even numbers
     */
    public void run() {
	while (true) {
	    System.out.println(">" + toNextEven());
	}
    }

}