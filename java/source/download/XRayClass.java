import java.lang.reflect.*;
import java.util.ArrayList;

public class XRayClass {

    public static void main(String[] args) {

	if (args.length != 2) {
	    System.out.println("Usage: " +
			       "java XRayClass <className> " +
			       "[declaredMethods | methods | " +
			       "declaredFields | fields]");
	    System.exit(1);
	}

	try {
	    Class<?> c = Class.forName(args[0]);

	    if (args[1].equals("declaredMethods")) {
		for (Method e : c.getDeclaredMethods()) 
		    System.out.println(e);
	    }
	    else if (args[1].equals("methods")) {
		for (Method e : c.getMethods()) 
		    System.out.println(e);
	    }
	    else if (args[1].equals("declaredFields")) {
		for (Field e : c.getDeclaredFields()) 
		    System.out.println(e);
	    }
	    else if (args[1].equals("fields")) {
		for (Field e : c.getFields()) 
		    System.out.println(e);
	    }
	    else {
		System.err.println("ERROR:" +
				   "The selected feature is not implemented");
	    }
	}
	catch (java.lang.ClassNotFoundException e) {
	    e.printStackTrace();
	}
    }
}
