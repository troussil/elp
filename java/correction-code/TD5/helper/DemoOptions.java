package helper; 

import helper.ProgramOptions; 
import helper.MissingOptionValue;
import helper.UnknownOption;

/**
 * Small client class that illustrates the use of ProgramOptions
 */
public class DemoOptions {
    
    /**
     * Main method
     * @param args array of strings storing the input option sequence
     */
    public static void main(String args[]) {

	ProgramOptions po = new ProgramOptions(); 
	//two declared options
	po.addOption("n"); 
	po.addOption("i"); 

	if (args.length == 0) {
	    po.displayAvailableOptions(); 
	} else {

	    try {

		po.parseCommandLine(args); 
		//some processing with these two options
		System.out.println("Do something with options"); 
		System.out.println("n value("+ po.getValue("n") +")"); 
		System.out.println("i value("+ po.getValue("i") +")"); 


	    } catch (Exception e) {
		e.printStackTrace();
	    } 

	}
    }
}
