/** Classe modelisant un interrupteur à bascule. */
class Interrupteur {
    /** booleen indiquant l'etat de l'interrupteur */
    boolean estEnMarche = false; 
    /** Methode basculant l'état de l'interrupteur. */
    void basculer() {
	estEnMarche = (!estEnMarche); 
    }
}