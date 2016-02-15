import java.io.*; 
import java.util.*; 

public class EcrireMessage {
    public static void main(String args[]) {
	if (args.length < 1) {
	    System.out.println("usage: java EcrireMessage <dest>"); 
	}
	else {

	    try {

		Message m = new Message( new Date(), "toto", "salut") ; 

		File dest = new File(args[0]);
		FileOutputStream fout = new FileOutputStream(dest); 
		ObjectOutputStream oout = new ObjectOutputStream(fout);

		oout.writeObject(m); 

		oout.close(); 
		fout.close(); 

	    } catch (FileNotFoundException e) {
		System.err.println("ERR: Le fichier source n'existe pas ou le fichier de destination ne peut être crée ou ouvert."); 
	    } catch (IOException e) {
		System.err.println("ERR: entrée/sortie."); 
	    } finally {
		System.out.println("END"); 
	    }	    
	}
    }
}