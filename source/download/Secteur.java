class Secteur {

    /** angles inf et sup du secteur */
    private double theta1, theta2; 

    /**
    * Calcul par modulo 2PI du representant
    * dans l'intervalle [0;2PI] d'un angle donne
    * @param angle un angle arbitraire
    * @return le meme angle recaler dans [0;2PI] 
    */
    private double recaler(double angle) {
	//TODO
    }

    /**
    * Constructeur
    * @param theta1 angle inferieur
    * @param theta2 angle superieur
    */
    public Secteur(double theta1, double theta2) {
	this.theta1 = recaler(theta1); 
	this.theta2 = recaler(theta2); 
    }

    /**
    * Ecart angulaire du secteur
    */
    public double ecart() {
	//TODO
    }

    /** 
    * Representation textuelle du secteur
    */
    public String toString() {
	//TODO
    }

}
