package tc.elp.java.datastructures; 

import junit.framework.TestCase;
import org.junit.Test;

/**
 * Class that tests DynamicArray class. 
 */
public class TestDynamicArray {
    /**
     * Check whether the input integer is a power of 2 or not
     * @param i any strictly positive integer
     * @return true if the input integer is a power of 2
     */
    private boolean isPowerOf2(int i) {
	while((i%2) == 0) 
	    i /= 2; 
	return (i == 1); 
    }

    /**
     * Simple test
     * @return false in case of failure, true otherwise
     */
    private boolean test() {
	int nb = 0; 
	int nbok = 0; 

	DynamicArray a = new DynamicArray(); 

	final int n = 256; 
	for (int i = 0; i < n; i++) {
	    a.push(i);
	    System.err.print(a.size() + "/" + a.capacity() + ", "); 
	    if (isPowerOf2(i+1)) {
		if ( (a.capacity() == a.size()) ) 
		    nbok++; 
		nb++; 
	    }
	}

	System.err.println(); 
	System.err.println(a); 

	for (int i = 0; i < n; i++) {
	    if (a.get(i) == i)
		nbok++; 
	    nb++; 
	}

	return (nb == nbok); 
    }

    /**
     * Call simple tests
     */
    @Test
    public void mainTest() {
	org.junit.Assert.assertTrue( test() ); 
    }
}
