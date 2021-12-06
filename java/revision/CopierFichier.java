import java.net.*;
import java.io.*;
import java.util.*; 

//--------------------------------------------------------------------
public class CopierFichier {

    public static void main(String [] args) {

        Collection<String> collec;

        try {
            //choix d'une collection
            if (args.length == 0)
                collec = new ArrayList<String>(); //choix par défaut
            else {
                Class<Collection<String> > classe = (Class<Collection<String> >) Class.forName(args[0]);
                collec = classe.getDeclaredConstructor().newInstance();
            }
            
            //lire et remplir collec
            int nbLues = 0; 
            URL url = new URL("https://liris.cnrs.fr/tristan.roussillon/ens/proc");
            InputStream is = url.openConnection().getInputStream();
            BufferedReader in = new BufferedReader( new InputStreamReader(is) );

            String ligne = in.readLine();
            while (ligne != null) {
                collec.add(ligne);
                nbLues++; 
                ligne = in.readLine();
            }

            //ecrire collec dans un fichier
            FileOutputStream fos = new FileOutputStream("copie.txt");
            PrintWriter out = new PrintWriter( new OutputStreamWriter(fos), true );
            for (String elt : collec) {
                out.println(elt); 
            }
            System.out.println(collec.size() + " lignes écrites / " + nbLues + " lignes lues");
            
        } catch(Exception e) {
            e.printStackTrace(); 
        }
    }
}
