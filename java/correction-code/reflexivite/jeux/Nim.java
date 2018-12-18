import fr.insalyon.tc.framework.JeuCombinatoire; 

/** 
 * Classe implementant le jeu de Nim. 
 * Deux joueurs enlevent alternativement 1, 2 ou 3 elements
 * d'un tas de 15 elements au depart. Le joueur qui ne peut
 * plus jouer a perdu. 
 */
public class Nim implements JeuCombinatoire {

    /** nombre a decremente */
    private int n = 15; 
    
    /** indique si le jeu est fini ('true') ou non ('false') */
    @Override
    public boolean estFini() {
        return (n <= 0); 
    }
    
    /** Coup realise par un joueur
     * @param unCoup coup d'un joueur (nombre d'elements retires)
     * @return 'true' si le coup est valide et que la position
     * a ete modifiee en consequence, 'false' sinon
     */
    @Override
    public boolean joueUnCoup(String unCoup) {
        int d = Integer.parseInt(unCoup);
        if ( (d > 0) && (d <= 3) && (d <= n) ) { //valide
            n -= d; 
            return true; 
        } else
            return false;
    }

    /**
     * Representation textuelle de la position courante
     */
    @Override
    public String toString() {
        return Integer.toString(n); 
    }
}
