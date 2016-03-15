public class Consumer implements Runnable {
    private CubbyHole myCubbyhole;
    public Consumer(CubbyHole c) {
	myCubbyhole = c;
    }
    public void run() {
	Product p;
	for (int i = 0; i < 10; i++) {
	    p = myCubbyhole.get();
	}
    }
}


