import java.io.*; 
import java.util.*; 

class LireMessage {
    public static void main(String args[]) {
	if (args.length < 1) {
	    System.out.println("usage: java LireMessage <src>"); 
	}
	else {

	    try {

		File src = new File(args[0]);
		FileInputStream fout = new FileInputStream(src); 
		ObjectInputStream oout = new ObjectInputStream(fout);

		Object m = oout.readObject(); 
		System.out.println(m); 

		oout.close(); 
		fout.close(); 

	    } catch (FileNotFoundException e) {
		System.err.println("ERR: Le fichier source n'existe pas ou le fichier de destination ne peut être crée ou ouvert."); 
	    } catch (IOException e) {
		System.err.println("ERR: entrée/sortie."); 
	    } catch (ClassNotFoundException e) {
		System.err.println("ERR: echec de la deserialisation"); 
	    } finally {
		System.out.println("END"); 
	    }
	    
	}
    }

}