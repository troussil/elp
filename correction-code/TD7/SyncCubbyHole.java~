public class SyncCubbyHole extends CubbyHole {

    public synchronized Product get() {
	Product res = null; 
	try {
	    if (myProduct == null)
		wait(); 

	    res = myProduct; 
	    myProduct = null; 
	    notifyAll(); 
	} catch (InterruptedException e) {
	    System.err.println("ERR: interrupted"); 
	}

	System.out.println("get " + res); 
	return res;
    }

    public synchronized void put(Product aProduct) {
	try {
	    if (myProduct != null) 
		wait(); 

	    myProduct = aProduct;
	    System.out.println("put " + myProduct); 
	    notifyAll(); 
	} catch (InterruptedException e) {
	    System.err.println("ERR: interrupted"); 
	}
    }
}

