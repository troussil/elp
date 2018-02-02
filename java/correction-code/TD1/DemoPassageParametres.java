/** Classe testant l'interrupteur à bascule. */
class DemoPassageParametres {

  static void faireBasculerBooleen(boolean unBool) {
    unBool = !unBool;
  }

  static void faireBasculerInterrupteur(Interrupteur unInterrupteur) {
    unInterrupteur.basculer();
  }

  public static void main(String[] args) {

    boolean b = false; //b == false
    faireBasculerBooleen(b); 
    System.out.println(b); //b == false

    Interrupteur i = new Interrupteur(); //i.estEnMarche == false
    faireBasculerInterrupteur(i); 
    System.out.println(i.estEnMarche); //i.estEnMarche == true

  }
}
