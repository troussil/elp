public class EvenCounterTest {

   public static void main(String args[]) {
	EvenCounter c = new EvenCounter(); 
	Thread t1 = new Thread(c); 
	Thread t2 = new Thread(c); 
	t1.start(); 
	t2.start(); 
    }

}