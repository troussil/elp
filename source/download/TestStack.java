package tc.elp.java.datastructures; 

import junit.framework.TestCase;
import org.junit.Test;

/**
 * Class that tests stack classes. 
 */
public class TestStack {
    /**
     * Generic test for a given stack
     * @param aStack any stack (which must be empty)
     * @return false in case of failure, true otherwise
     */
    public boolean test(Stack aStack) {
	int nb = 0; 
	int nbok = 0; 

	try {

	    if (aStack.empty()) 
		nbok++; 
	    nb++; 

	    int n = 10; 
	    for (int i = n; i >= 1; i--)
		aStack.push(i);

	    if (!aStack.empty()) 
		nbok++; 
	    nb++; 
	
	    System.out.println(aStack); 

	    for (int i = 1; i <= n; i++)
		{
		    if (aStack.top() == i) 
			nbok++; 
		    nb++; 
		    aStack.pop(); 
		}

	    if (aStack.empty()) 
		nbok++; 
	    nb++; 

	} catch (EmptyStackException e) {
	    e.printStackTrace(); 
	}

	return ( (nb>0)&&(nb == nbok) ); 
    }

    /**
     * Call simple test
     */
    @Test
    public void mainTest() {
	//TODO
	//create an instance s of StackByLinkedList (or StackByArray)
	//test it by calling: 
	//org.junit.Assert.assertTrue( test(s)&&... ); 
    }
}
