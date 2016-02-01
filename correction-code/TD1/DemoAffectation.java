/** Classe executable montrant la difference de
comportement entre les types primitifs et personnalisés
face à l'affectation. */
class DemoAffectation {
  public static void main(String[] args) {

  boolean b1 = false;
  boolean b2 = b1;
  b2 = !b2;
  System.out.println( b1 ); //b1 == false

  Interrupteur i1 = new Interrupteur();
  Interrupteur i2 = i1;
  i2.basculer();
  System.out.println( i1.estEnMarche ); //true

  }
}
