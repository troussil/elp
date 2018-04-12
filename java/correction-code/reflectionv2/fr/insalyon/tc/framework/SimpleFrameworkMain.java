package fr.insalyon.tc.framework;
import fr.insalyon.tc.framework.Animal;
import java.lang.reflect.*;
    
class AnimalProxyFactory {
 
    public static Animal getAnimalProxy(final Animal a) {
        return (Animal) Proxy.newProxyInstance
	    (a.getClass().getClassLoader(),
	     new Class[] { Animal.class },
	     new InvocationHandler() {
		 public Object invoke(Object proxy, Method method, Object[] args)
		     throws Throwable {
		     StringBuffer sb = new StringBuffer();
		     sb.append(method.getName()); sb.append("(");
		     for (int i=0; args != null && i<args.length; i++) {
			 if (i != 0)
			     sb.append(", ");
			 sb.append(args[i]);
		     }
		     sb.append(")");
		     Object ret = method.invoke(a, args);
		     if (ret != null) {
			 sb.append(" -> "); sb.append(ret);
		     }
		     System.err.println(sb);
		     return ret;
		 }
	     });
    }
}

//imagine this is the entry point for a framework, this can not be changed
public class SimpleFrameworkMain {

    public static void main(String[] args) {

	if (args.length < 1) {
	    System.err.println("You must provide the name of a class that extends Animal"); 
	} else {
	    String yourClassName = args[0]; 

	    try {
		
		Class<?> yourClass = Class.forName(yourClassName);  
		Animal a = AnimalProxyFactory.getAnimalProxy( (Animal) yourClass.newInstance() ); 
		System.out.println(yourClassName + ": " + a.scream());

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
