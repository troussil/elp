/** Stack interface  */
public interface Stack {
    /** 
     * Check if the list is empty
     * @return 'true' if empty, 'false' otherwise
     */
    boolean empty(); 
    /**
     * Get the value that is on the top of the stack
     * @return the top value 
     */
    int top(); 
    /**
     * Add a value at the top of the stack
     * @param aValue new value
     */
    void push(int aValue); 
    /** Remove the value that is on the top of the stack */
    void pop(); 
}
