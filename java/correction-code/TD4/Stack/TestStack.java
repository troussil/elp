/**
 * Class that tests stack classes. 
 */
public class TestStack {
    /**
     * Generic test for a given stack
     * @param aStack any stack (which must be empty)
     * @return false in case of failure, true otherwise
     */
    public static boolean test(Stack aStack) {
	int nb = 0; 
	int nbok = 0; 


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

	return ( (nb>0)&&(nb == nbok) ); 
    }

    /**
     * Call simple test
     */
    public static void main(String[] args) {

	StackByArray s = new StackByArray(); 
	
	String res = (test(s))?"SUCCES":"ECHEC"; 
	System.out.println(res); 

    }
}
