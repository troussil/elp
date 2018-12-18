package fr.insalyon.tc.framework; 

/** 
 * Interface listant les methodes d'un jeu combinatoire
 * a deux joueurs. Les joueurs jouent un coup alternativement
 * tant que c'est encore possible. Le joueur qui ne peut plus
 * jouer a perdu. 
 */
public interface JeuCombinatoire {
    /** indique si le jeu est fini ('true') ou non ('false') */
    public boolean estFini();
    /** Coup realise par un joueur
     * @param unCoup coup d'un joueur
     * @return 'true' si le coup est valide et que la position
     * a ete modifiee en consequence, 'false' sinon
     */
    public boolean joueUnCoup(String unCoup);   
}
