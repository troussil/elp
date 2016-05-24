import java.util.Queue; 
import java.util.LinkedList; 
import java.util.NoSuchElementException; 
/**
 * Class that implements a recursive solution
 * to the Tower of Hanoi.
 */
class Hanoi {
    /** 
     * Take the upper element from @a src
     * and place it on top of @a dest
     * @param src source stack
     * @param dest destination stack
     */
    private static void oneMove(Queue<Integer> src, Queue<Integer> dest) 
	throws NoSuchElementException {
	dest.add( src.remove() ); 
    }
    /** 
     * Move the whole stack @a src to position @a dest
     * @param src source stack
     * @param dest destination stack
     * @param tmp middle stack
     * @pre src the source stack must not be empty 
     */
    public static void move(Queue<Integer> src, Queue<Integer> dest, Queue<Integer> tmp) 
	throws NoSuchElementException {
	oneMove(src, tmp); 
	if ( src.size() != 0 ) 
	    move(src, dest, tmp); //recursive call
	oneMove(tmp, dest);
    }

    public static void display(Queue<Integer> q1, Queue<Integer> q2, Queue<Integer> q3) {
	display(q1); 
	display(q2); 
	display(q3); 
    }

    public static void display(Queue<Integer> q) {
	System.out.print("|--"); 
	for (Integer i : q) {
	    System.out.print(i + ", "); 
	}
	System.out.println(">"); 
    }

    public static void main(String[] args) {

	Queue<Integer> a, b, c; 
	a = new LinkedList<Integer>(); 
	b = new LinkedList<Integer>(); 
	c = new LinkedList<Integer>(); 

	//we fill a with values ranging from max to 1
	final int max = 5; 
	for (int i = max; (i >= 1); i--)
	    a.add(i); 
	//we leave b and c empty
	
	Hanoi.display(a, b, c);
	System.out.println("move..."); 
	Hanoi.move(a, b, c); 
	Hanoi.display(a, b, c); 

	System.out.println("clear..."); 
	b.clear(); 
	//what next?
	try {

	    Hanoi.display(a, b, c);
	    System.out.println("move..."); 
	    Hanoi.move(a, b, c); 
	    Hanoi.display(a, b, c); 

	} catch (NoSuchElementException e) {
	    System.err.println("Exception catched if queues are empty"); 
	} finally {
	    System.err.println("END"); 
	}

    }

}
