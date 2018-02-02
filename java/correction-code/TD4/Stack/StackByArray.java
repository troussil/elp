/**
 * Class that implements a stack of integers
 * by a dynamic array  
 */
public class StackByArray extends DynamicArray implements Stack {

    /** Default constructor */
    public StackByArray() {
	super();  
    }

    /** 
     * Checks if the list is empty
     * @return 'true' if empty, 'false' otherwise
     */
    public boolean empty() {
	return (size() == 0); 
    }

    /**
     * Gets the value that is on the top of the stack
     * @return the top value 
     */
    public int top() {
	return get(size()-1); 
    }

    /**
     * Removes the value that is on the top of the stack
     */
    public void pop() {
	if (mySize > 0)
	    mySize--;  
    }

    /**
     * Overriding of toString
     * @return the content of the stack (from top to bottom)
     */
    @Override
    public String toString() {
	String res = "["; 
        for (int i = (size()-1); i >= 0; i--) {
	    res += get(i) + ","; 
	}
	res += "--]"; 
	return res; 
    }

}
