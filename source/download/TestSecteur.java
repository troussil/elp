class TestSecteur {
    public static void main(String[] args) {
	int nb = 0; //nombre total de tests
	int nbok = 0; //nombre de tests reussis
	double epsilon = 0.00001; //marge d'approximation

	//premier secteur
	Secteur s1 = new Secteur(10*java.lang.Math.PI/2,-9*java.lang.Math.PI/2); 
	System.out.println("[10pi/2:-9pi/2[ == [pi:3pi/2[ == "+s1+" a une etendue de pi/2 == "+s1.ecart());  
	if ( (s1.ecart() <= java.lang.Math.PI/2 + epsilon)
	     && (s1.ecart() > java.lang.Math.PI/2 - epsilon) )
	    nbok++; 
	nb++; 
	    
	//second secteur
	Secteur s2 = new Secteur(java.lang.Math.PI,java.lang.Math.PI/2); 
	System.out.println("[pi:pi/2[ == "+s2+" a une etendue de 3pi/2 == "+s2.ecart()); 
	if ( (s2.ecart() <= 3*java.lang.Math.PI/2 + epsilon)
	     && (s2.ecart() > 3*java.lang.Math.PI/2 - epsilon) )
	    nbok++; 
	nb++; 
 
	String res = (nb == nbok) ? "SUCCES" : "ECHEC"; 
	System.out.println(res); 
    }
}