import fr.insalyon.tc.framework.JeuCombinatoire; 

import java.util.List; 
import java.util.ArrayList; 

import java.io.File;

/** 
 * Classe implementant un jeu combinatoire composes d'un 
 * ensemble de differents jeux combinatoires.  
 * Deux joueurs jouent alternativement en choisissant un jeu 
 * et un coup pour ce jeu. Quand il n'y a plus la possibilite
 * de jouer dans un jeu, il est ignore et la partie se poursuit
 * sur les autres jeux. Le joueur qui ne peut plus jouer dans 
 * le dernier jeu non fini a perdu. 
 */
public class EnsembleDeJeux implements JeuCombinatoire {

    /** liste des jeux */
    private List<JeuCombinatoire> jeux = new ArrayList<JeuCombinatoire>();

    /** Constructeur par défaut de l'ensemble de jeux */
    public EnsembleDeJeux() {

        //on parcourt les répertoires du classpath
        String nomRepertoire = System.getProperty("java.class.path");

        //pour chaque fichier ".class"
        //(mais different du byte code de cette classe)
        String ext = ".class"; 
        File repertoire = new File(nomRepertoire); 
        for (String nom : repertoire.list()) {
            if ( (nom.toLowerCase().endsWith(ext)) &&
                 (!nom.equals(this.getClass().getName() + ext)) ) {
                        
                String nomClasse = nom.substring(0,nom.length() - ext.length());
                System.out.println(nomClasse); 

                try {
                    //ajouter le jeu detecte a la liste
                    Class<?> laClasse = Class.forName(nomClasse); 
                    JeuCombinatoire jeu = (JeuCombinatoire) laClasse.newInstance();  
                    System.out.println("Ajouter jeu " + laClasse.getName());
                    jeux.add(jeu);
                        
                    //on ignore les exceptions
                } catch (ClassNotFoundException e) {
                    System.err.println("warning : Classe '" + nomClasse + "' non trouvee"); 
                } catch (InstantiationException e) {
                    System.err.println("warning : Classe '" + nomClasse + "' non instanciable et non ajoutee"); 
                } catch (IllegalAccessException e) {
                    System.err.println("warning : Classe '" + nomClasse + "' non utilisable et non ajoutee");
                }
                    
            }    
        }

        if (jeux.isEmpty())
            throw new RuntimeException("Erreur : aucun jeu trouve"); 
    }
    
    /** indique si le jeu est fini ('true') ou non ('false') */
    @Override
    public boolean estFini() {
        int c = 0; //compte le nombre de jeux finis
        for (JeuCombinatoire jeu : jeux)
            if (jeu.estFini())
                c++; 
        return (c == jeux.size()); //tous les jeux sont finis
    }
    
    /** Coup realise par un joueur
     * @param unCoup coup d'un joueur. Il est de la forme "num:coupJeuNum",
     * où 'num' indique l'indice du jeu dans la liste courante et 
     * 'coupJeuNum' est le coup pour le jeu d'indice 'num'. 
     * @return 'true' si le coup est valide et que la position
     * (dans le jeu d'indice 'num') a ete modifiee en consequence, 
     * 'false' sinon
     */
    @Override
    public boolean joueUnCoup(String unCoup) {
        
        String[] msg = unCoup.split(":");
        if (msg.length == 2) {
            int num = Integer.parseInt(msg[0]);
            return ( jeux.get(num).joueUnCoup( msg[1] ) ); 
        } else 
            return false;
    }

    /**
     * Representation textuelle de la position courante
     */
    @Override
    public String toString() {
        String res = "";
        int c = 0; 
        for (JeuCombinatoire jeu : jeux) {
            res += ">> Position du jeu #" + c + "\n";
            res += jeu.toString();
            res += "\n";
            c++;
        }       
        return res; 
    }
}
