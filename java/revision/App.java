import java.io.*;
import java.net.*; 
import java.util.*; 

import com.google.gson.*; 

public class App {

    private static final String vortaroPath = "http://www.simplavortaro.org/api/v1/vorto/";

    //--------------------------------------------------------------------------
    private static List<String> getWordListFromFile(String filename)
	throws FileNotFoundException, IOException {
	
	ArrayList<String> lst = new ArrayList<String>(); 
		
	FileInputStream fis = new FileInputStream(filename);
	BufferedReader in = new BufferedReader( new InputStreamReader(fis) );
	   
	String line = in.readLine();
	while (line != null) {
	    lst.add(line); 
	    line = in.readLine(); 
	}
	return lst; 
    }
    
    //--------------------------------------------------------------------------
    private static String getWordFromList(List<String> lst) {
	Random random = new Random();
	int idx = random.nextInt(lst.size());
	return lst.get(idx);
    }

    //--------------------------------------------------------------------------
    private static String getDataFromWord(String word)
	throws MalformedURLException, IOException {
	
	String request = vortaroPath + word;

	URL url = new URL(request);
	URLConnection connection = url.openConnection();
	InputStream is = connection.getInputStream();
	BufferedReader in = new BufferedReader( new InputStreamReader(is) );
	return in.readLine();
    }

    //--------------------------------------------------------------------------
    private static void printDefinitions(String data) {
	// Creates new instance of Gson
	Gson gson = new Gson();
	// Converts the json string to JsonElement without POJO 
	JsonElement element = gson.fromJson (data, JsonElement.class);
	// Converting JsonElement to JsonObject
	JsonObject jsonObj = element.getAsJsonObject();
	// Get the fields containing the definitions
	// according to the vortaro API
	JsonArray defs = jsonObj.get("difinoj").getAsJsonArray();
	for (JsonElement o: defs) {
	    JsonElement def = o.getAsJsonObject().get("difino"); 
	    System.out.println(def);
	}
    }

    //--------------------------------------------------------------------------
    private static void guessLoop(String word)
	throws IOException {

	System.out.println("Proposez un mot correspondant aux definitions"
			   + " (ou 'q' pour quitter)"); 
	BufferedReader in = new BufferedReader( new InputStreamReader(System.in) );
	String line = in.readLine();
	while ( (line != null) && (! line.equals("q")) ) {
	    if (line.equals(word)) {
		System.out.println("Bravo, vous avez trouve!");
		line = "q"; 
	    } else {
		System.out.println("Non, essayez encore.");
		line = in.readLine(); 
	    }
	}
	System.out.println("Le mot a deviner etait " + word); 
    }
    
    //--------------------------------------------------------------------------
    public static void main(String[] args) {

	try {
	    List<String> words = getWordListFromFile(args[0]);
	    String word = getWordFromList(words);
	    String data = getDataFromWord(word);
	    printDefinitions(data);
	    guessLoop(word); 

	} catch (ArrayIndexOutOfBoundsException e) {
	    System.err.println("you should provide a file name as the first argument");
	} catch (FileNotFoundException e) {
	    System.err.println("specified file not found");
	} catch (MalformedURLException e) {
	    System.err.println("not valid URL");
	} catch (IOException e) {
	    System.err.println("input/output error");
	}
    }

}
