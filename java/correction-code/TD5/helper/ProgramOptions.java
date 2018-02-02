package helper; 

import java.util.Set; 
import java.util.HashSet; 
import java.util.Map; 
import java.util.HashMap; 

import helper.MissingOptionValue;
 
/**
 * Small tool for option management.
 * Options are given as a sequence of strings where 
 * two consecutive strings must be a pair of the 
 * following form: 
 * option name (prefixed by '-') and corresponding value, 
 * e.g. -optionM valueOfOptionM -optionN valueOfOptionN
 */
public class ProgramOptions {

    /// set of available options
    private Set<String> myAvailableOptions = new HashSet<String>(); 
    /// mapping of the option names and their corresponding values given by the user
    private Map<String,String> myGivenOptionsAndValues = new HashMap<String,String>(); 

    /**
     * Adds an option name to the set of available options
     * @param optionName name of the option
     */
    public void addOption(String optionName) {
        myAvailableOptions.add(optionName); 
    }

    /**
     * Returns the value of a given option. 
     * @param optionName name of the option
     * @return the corresponding value (null if the given 
     * option does not exist) 
     */
    public String getValue(String optionName) { 
	return myGivenOptionsAndValues.get(optionName);
    }

    /**
     * Parses the option sequence and stores the pairs 
     * option name - value in myGivenOptionsAndValues
     * @parap args array of strings containing the option sequence
     */ 
    public void parseCommandLine(String[] args) throws MissingOptionValue, UnknownOption {

	int i = 0;
	while ( i < args.length) {
	    String s = args[i]; 
	    if (s.charAt(0) == '-') { 
		String option = s.substring(1,s.length()); //option name without '-' 
		if (myAvailableOptions.contains(option)) {
		    i++; 
		    if (i < args.length) {
			String value = args[i]; 
			if ( (!args[i].isEmpty()) && (args[i].charAt(0) != '-') )
			    myGivenOptionsAndValues.put(option, args[i]);
			else
			    throw new MissingOptionValue(option); 
		    } else 
			throw new MissingOptionValue(option); 
		} else 
		    throw new UnknownOption(option); 
	    } //else unknown token
	    i++; 
	}

	System.err.println("Given options: " + myGivenOptionsAndValues); 
    }

    /**
     * Prints to standard output the set of available options
     */
    public void displayAvailableOptions() {
	System.out.println("Available options are: "); 
	for (String s : myAvailableOptions) {
	    System.out.println(" " + s); 
	}
    }

}
