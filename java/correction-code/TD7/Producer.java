public class Producer implements Runnable {
    private CubbyHole myCubbyhole;
    public Producer(CubbyHole c) {
	myCubbyhole = c;
    }
    public void run() {
	for (int i = 0; i < 10; i++) {
	    myCubbyhole.put( new Product(i) );
	}
    }
}
