import java.io.*;
import java.util.*;
import java.net.*;

//--------------------------------------------------------------------
class Annuaire {

    public Map<String,String> map; 

    public Annuaire(String filename)
	throws FileNotFoundException, IOException, RuntimeException {

	map = new HashMap<String,String>();
	
	FileInputStream fis = new FileInputStream(filename);
	BufferedReader in = new BufferedReader( new InputStreamReader(fis) );

	String line = in.readLine();
	while (line != null) {
	    String[] parts = line.split(";");
	    if (parts.length != 2) {
		throw new RuntimeException(); 
	    }
	    map.put(parts[0], parts[1]);
	    line = in.readLine(); 
	}
    }

    @Override
    public String toString() {
	return map.toString(); 
    }
}

//--------------------------------------------------------------------
class AnnuaireServeur {

    private static final String filename = "source.txt";
    private static final int numeroPort = 8080;

    public static void main(String[] args) {

	try {
	    
	    Annuaire annuaire = new Annuaire(filename); 
	    System.out.println(">> annuaire: " + annuaire);
	    	    
	    ServerSocket connection = new ServerSocket(numeroPort);
	    Socket socket = connection.accept();
	    
	    InputStream is = socket.getInputStream();
	    BufferedReader in = new BufferedReader( new InputStreamReader(is) );

	    OutputStream os = socket.getOutputStream();
	    PrintWriter out = new PrintWriter( new OutputStreamWriter(os), true );

	    String line = in.readLine();
	    while (line != null) {
		String email = annuaire.map.get(line); 
		out.println(email);
		line = in.readLine(); 
	    }

	}
	catch (FileNotFoundException e) {
	    System.err.println("file " + filename + " not found"); 
	}
	catch (IOException e) {
	    System.err.println("file " + filename + " cannot be read"); 
	}
	catch (RuntimeException e) {
	    System.err.println("file " + filename + ": bad format"); 
	}
    }
}
