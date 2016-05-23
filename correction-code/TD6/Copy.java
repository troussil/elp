import java.io.IOException; 
import java.io.FileNotFoundException; 
import java.io.InputStream; 
import java.io.OutputStream;
import java.io.File; 
import java.io.FileInputStream; 
import java.io.FileOutputStream; 

class Copy {

    private static void copy(InputStream is, OutputStream os) throws java.io.IOException {
	int octet = is.read(); 
	while (octet != -1) {
	    os.write(octet); 
	    octet = is.read(); 
	}
    }

    public static void main(String [] args) {

	InputStream is; 
	OutputStream os; 

	try {

	    switch (args.length) {
	    case 0: 
		is = System.in; 
		os = System.out; 
		break; 
	    case 1: 
		is = new FileInputStream(new File(args[0]));
		os = System.out; 
		break; 
	    case 2: 
		is = new FileInputStream(new File(args[0]));
		os = new FileOutputStream(new File(args[1]));
		break; 
	    default: 
		is = new FileInputStream(new File(args[0]));
		os = new FileOutputStream(new File(args[1]));
		System.err.println("WARNING: too much arguments"); 
		System.err.println("usage: java Copy source.txt cible.txt"); 
	    }

	    copy(is, os); 

	    is.close(); 
	    os.close(); 

	} catch(FileNotFoundException e) {
	    System.err.println("ERR: files not found");
	} catch(IOException e) {
	    System.err.println("ERR: input/output error");
	}

    }

}