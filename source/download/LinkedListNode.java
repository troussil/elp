package tc.elp.java.datastructures; 
/**
 * Class that implements a linked list node,
 * which is able to contain an integer.  
 */
class LinkedListNode {

    /** value of the node */
    private int myValue; 
    /** reference to the next node */
    private LinkedListNode myNext; 

    /**
     * Construct a single new node
     * @param aValue value of the node
     */
    public LinkedListNode(int aValue) {
	myValue = aValue; 
	myNext = null; 
    }

    /**
     * Construct a linked new node
     * @param aValue value of the node
     * @param aNode next node
     */
    public LinkedListNode(int aValue, LinkedListNode aNode) {
	myValue = aValue; 
	myNext = aNode; 
    }

    /** 
     * Get the node value
     * @return value of the node
     */
    public int value () {
        return myValue;
    }

    /** 
     * Get the next node 
     * @return next node (possibly null)
     */
    public LinkedListNode next () {
        return myNext;
    }

    /** 
     * @return a string representation of the node value
     */
    public String toString () {
        return Integer.toString(myValue);
    }

}
