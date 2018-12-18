import java.lang.reflect.*;
import java.util.ArrayList;

public class XRayClass {

    public <T> Method[]
        getDeclaredMethodsFromClass(Class<T> c) {

        Method[] mList;

        mList = c.getDeclaredMethods();
        return mList;
    }

    public <T> Method[]
        getMethodsFromClass(Class<T> c) {

        Method[] mList;

        mList = c.getMethods();
        return mList;
    }

    public <T> Constructor[]
        getAllConstructorsFromClass(Class<T> c) {

        Constructor[] cList;

        cList = c.getDeclaredConstructors();
        return cList;
    }

    public <T> Field[]
        getFieldsFromClass(Class<T> c) {
        Field[] fList;

        fList = c.getDeclaredFields();
        return fList;
    }

    public <T> ArrayList<Class<? super T> >
        getClassesFromClass(Class<T> c) {
        ArrayList<Class<? super T> > cList = new ArrayList<Class<? super T> >();
        Class<? super T>  cTemp;

        cTemp = c;
        while ((cTemp =
                cTemp.getSuperclass()) != null) {
            cList.add(cTemp);
        }

        return cList;
    }

    public <T> Class<?>[]
        getInterfacesFromClass(Class<T> c) {

        Class<?>[] cList;

        cList = c.getInterfaces();
        return cList;
    }

    public static void main(String[] args) {

        XRayClass xray = new XRayClass();

        if (args.length != 2) {
            System.out.println("Usage: " +
                               "java XRayClass <className> " +
                               "[declaredMethods | methods | " +
                               "constructors | fields | inheritance |" +
                               "interfaces] ");
            System.exit(1);
        }

        try {
            Class<?> c = Class.forName(args[0]);

            if (args[1].equals("declaredMethods")) {
                for (Method e : xray.getDeclaredMethodsFromClass(c)) 
                    System.out.println(e);
            }
            else if (args[1].equals("methods")) {
                for (Method e : xray.getMethodsFromClass(c)) 
                    System.out.println(e);
            }
            else if (args[1].equals("Constructors")) {
                for (Constructor e : xray.getAllConstructorsFromClass(c)) 
                    System.out.println(e);
            }
            else if (args[1].equals("fields")) {
                for (Field e : xray.getFieldsFromClass(c)) 
                    System.out.println(e);
            }
            else if (args[1].equals("inheritance")) {
                for (Class<?> e : xray.getClassesFromClass(c)) 
                    System.out.println(e);
            }
            else if (args[1].equals("interfaces")) {
                for (Class<?> e : xray.getInterfacesFromClass(c)) 
                    System.out.println(e);

            }
            else {
                System.out.println("ERROR:" +
                                   "The selected feature is not implemented");
            }
        }
        catch (java.lang.ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
}
