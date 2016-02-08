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
	System.out.println(nbok+"/"+nb); 
    
	//second secteur
	Secteur s2 = new Secteur(java.lang.Math.PI,java.lang.Math.PI/2); 
	System.out.println("[pi:pi/2[ == "+s2+" a une etendue de 3pi/2 == "+s2.ecart()); 
	if ( (s2.ecart() <= 3*java.lang.Math.PI/2 + epsilon)
	     && (s2.ecart() > 3*java.lang.Math.PI/2 - epsilon) )
	    nbok++; 
	nb++; 
	System.out.println(nbok+"/"+nb); 

	//secteur complet
	Secteur s3 = Secteur.creerComplet(); 
	System.out.println("secteur complet == "+s3+" a une etendue de 2pi == "+s3.ecart()); 
	if ( (s3.ecart() <= (2*java.lang.Math.PI) + epsilon)
	     && (s3.ecart() > (2*java.lang.Math.PI) - epsilon) )
	    nbok++; 
	nb++; 
	System.out.println(nbok+"/"+nb); 

	//premier quadrant
	Secteur s4 = Secteur.creerPremierQuadrant(); 
	System.out.println("premier quadrant == "+s4+" a une etendue de pi/2 == "+s4.ecart()); 
	if ( (s4.ecart() <= java.lang.Math.PI/2 + epsilon)
	     && (s4.ecart() > java.lang.Math.PI/2 - epsilon) )
	    nbok++; 
	nb++; 
	System.out.println(nbok+"/"+nb); 

	//secteur vide
	Secteur s5 = new Secteur(0.0,0.0); 
	System.out.println("secteur vide == "+s5+" a une etendue de 0 == "+s5.ecart()); 
	if ( (s5.ecart() <=  epsilon)
	     && (s5.ecart() > -epsilon) )
	    nbok++; 
	nb++; 
	System.out.println(nbok+"/"+nb); 

	String res = (nb == nbok) ? "SUCCES" : "ECHEC"; 
	System.out.println(res); 
    }
}