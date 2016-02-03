public class DemoComplexe {

     public static void main(String[] args) {

	 try {

	 Complexe o = new Complexe(0,0);
	 Complexe c = new Complexe(2,1);
	 System.out.print(c + " / " + o + " == "); 
	 c.diviser(o); 
	 System.out.println(c); 

	 }
	 catch (DivisionComplexeParZero e) {
	     e.printStackTrace(); 
	 }
    }

}