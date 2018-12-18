import fr.insalyon.tc.framework.JeuCombinatoire; 

/** 
 * Classe implementant le jeu de Wythoff. 
 * Deux joueurs deplacent alternativement un pion 
 * sur un damier, d'autant de cases qu'ils le souhaitent 
 * - soit horizontalement vers la gauche, 
 * - soit verticalement vers le bas, 
 * - soit en diagonal vers la gauche et le bas. 
 * Le joueur qui ne peut plus jouer, car le pion 
 * se trouve dans le coin en bas à gauche du damier,
 * a perdu. 
 */
public class Wythoff implements JeuCombinatoire {

    /** taille du damier */
    private int maxx = 9;
    private int maxy = 9; 
    /** position de depart */
    private int x = 7;
    private int y = 4;

    /** indique si le jeu est fini ('true') ou non ('false') */
    public boolean estFini() {
        return ( (x <= 0)&&(y <= 0) ); 
    }
    
    /** Coup realise par un joueur
     * @param unCoup coup d'un joueur sous la forme "i,j", 
     * où i et j sont les coordonnées de la nouvelle position
     * (l'origine "0,0" est le coin en bas a gauche).
     * @return 'true' si le coup est valide et que la position
     * a ete modifiee en consequence, 'false' sinon
     */
    public boolean joueUnCoup(String unCoup) {
        boolean res = true;
        
        String[] coords = unCoup.split(",");
        if (coords.length == 2) {
            int i = Integer.parseInt(coords[0]);
            int j = Integer.parseInt(coords[1]);
            if ( ( (i >= 0) && (j >= 0) ) &&         //dans le damier
                 ( ( (i == x) && (j < y) ) ||           //horizontale
                   ( (j == y) && (i < x) ) ||           //verticale
                   ( (i == j) && (i < x) && (j < y) ) ) //diagonale
                 ) { //valide
                x = i;
                y = j;
            } else {
                res = false;
            }
        } else {
            res = false;
        }
        
        return res; 
    }
    
    /**
     * Representation textuelle de la position courante
     */
    @Override
    public String toString() {
        String res = "";
        for (int j = maxy-1; j >= 0; j--) {
            res += j + " "; 
            for (int i = 0; i < maxx; i++) {
                if ( (x == i) && (y == j) )
                    res += "X "; 
                else
                    res += "_ ";
            }
            res += "\n";
        }
        res += "  0 1 2 3 4 5 6 7 8"; 
        return res; 
    }
}
