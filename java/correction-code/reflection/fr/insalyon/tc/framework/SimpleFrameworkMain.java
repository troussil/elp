package fr.insalyon.tc.framework;
import fr.insalyon.tc.framework.Animal;

//imagine this is the entry point for a framework, this can not be changed
public class SimpleFrameworkMain {

    public static void main(String[] args) {

	if (args.length < 1) {
	    System.err.println("You must provide the name of a class that extends Animal"); 
	} else {
	    String yourClassName = args[0]; 

	    try {
		
		Class<?> yourClass = Class.forName(yourClassName);  
		Animal a = (Animal) yourClass.newInstance();  
		System.out.println(yourClassName + " " + a.scream());

	    } catch (ClassNotFoundException e) {
		System.err.println("Class called '" + yourClassName + "' not found"); 
	    } catch (InstantiationException e) {
		System.err.println("Class called '" + yourClassName + "' cannot be instantiated"); 
	    } catch (IllegalAccessException e) {
		System.err.println("Class called '" + yourClassName + "' cannot be used"); 
	    }
	}
    }
}
