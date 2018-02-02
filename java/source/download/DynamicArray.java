/**
 * Class that implements a dynamic array of integers. 
*/
class DynamicArray {

    /** size of the dynamic array */
    protected int mySize; 
    /** underlying static array */
    private int[] myArray; 

    /** Default constructor */
    public DynamicArray() {
	myArray = new int[1]; 
    }

    /** 
     * Insert a new integer into the array. 
     * A new underlying array is allocated if 
     * the number of integers exceed the array capacity. 
     * @param aValue value to push into the array
     * @warning O(n)
     */
    public void push(int aValue) {
	mySize++; 
	if (mySize > myArray.length) {
	    //increase the size
	    int[] anArray = new int[myArray.length*2]; 
	    //copy
	    java.lang.System.arraycopy(myArray, 0, anArray, 0, myArray.length);
	    myArray = anArray; 
	} 
	myArray[mySize-1] = aValue; 
    }

    /** 
     * Insert a new integer at index @a aIndex 
     * @param aIndex index where to set the value
     * @param aValue value to push into the array
     * @warning O(1)
     */
    public void push(int aIndex, int aValue) {
	myArray[aIndex] = aValue; 
    }

    /**
     * Get the integer stored at index @a aIndex
     * @param aIndex index
     * @param the corresponding integer
     */
    public int get(int aIndex) {
	return myArray[aIndex]; 
    }

    /**
     * Get the size of the dynamic array, 
     * ie. its number of integers
     * @return the size
     */
    public int size() {
	return mySize;  
    }

    /**
     * Get the storage capacity, 
     * ie. the size of the underlying static array.
     * @return capacity 
     */
    public int capacity() {
	return myArray.length;  
    }

    /**
     * Overriding of toString
     * @return the content of the dynamic array 
     * (the mySize first integers)
     */
    @Override
    public String toString() {
	String res = "["; 
        for (int i = 0; i < mySize; i++) {
	    res += myArray[i] + ","; 
	}
	res += "--]"; 
	return res; 
    }

}
