package fr.insalyon.tc.framework;
import fr.insalyon.tc.framework.JeuCombinatoire;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException; 

/** Classe executable qui lancent les jeux */
public class Main {

    public static void main(String[] args) {

        BufferedReader in = new BufferedReader( new InputStreamReader( System.in ) );
        boolean stop = false; 
        while ( ! stop ) {

            String nomClasse = "";
            
            try {
                System.out.println("Entrez un nom de classe ou 'q' pour quitter");
                nomClasse = in.readLine();
                
                if (! nomClasse.equals("q")) {
        
                    Class<?> votreClasse = Class.forName(nomClasse);  
                    JeuCombinatoire jeu = (JeuCombinatoire) votreClasse.newInstance();  
                    System.out.println("Vous jouez a " + votreClasse.getName());

                    String playerName[] = { "Joueur A", "Joueur B" }; 
                    int c = 0; //compteur
                    //tant que le jeu n'est pas fini
                    while (! jeu.estFini() ) {
                        //affichage de la position courante
                        System.out.println(">> Position courante");
                        System.out.println(jeu);
                        //le joueur courant joue
                        System.out.println(">> " + playerName[c%2]);
                        if ( jeu.joueUnCoup( in.readLine() ) )
                            c++;
                        else 
                            System.err.println(">> Coup invalide, a refaire");  
                    }
                    //quand le jeu est fini,
                    //affichage de la position courante
                    System.out.println(">> Position courante");
                    System.out.println(jeu);
                    //decision quant a l'issue du jeu
                    System.out.println(">> " + playerName[c%2] + " a perdu!");
                    
                } else {
                    stop = true;
                }
                
            } catch (ClassNotFoundException e) {
                System.err.println("Erreur : classe '" + nomClasse + "' non trouvee"); 
            } catch (InstantiationException e) {
                System.err.println("Erreur : classe '" + nomClasse + "' non instanciable"); 
            } catch (IllegalAccessException e) {
                System.err.println("Erreur : classe '" + nomClasse + "' non utilisable");
            } catch (IOException e) {
                System.err.println("Erreur : entree-sortie lors du jeu"); 
            } catch (RuntimeException e) {
                System.err.println("Erreur : jeu indisponible"); 
            }

        }
    }
}
