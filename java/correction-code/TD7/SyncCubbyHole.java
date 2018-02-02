public class SyncCubbyHole extends CubbyHole {

    public synchronized Product get() {
	Product res = null; 
	try {
	    while (myProduct == null)
		wait(); 
	    //maintenant myProduct != null
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
	    while (myProduct != null) 
		wait(); 
	    //maintenant myProduct == null
	    myProduct = aProduct;
	    System.out.println("put " + myProduct); 
	    notifyAll(); 
	} catch (InterruptedException e) {
	    System.err.println("ERR: interrupted"); 
	}
    }
}

