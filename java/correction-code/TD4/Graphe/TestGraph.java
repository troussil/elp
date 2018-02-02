public class TestGraph {

    public static void main(String[] args) {

	int nb = 0; //total number of tests 
	int nbok = 0; //number of passed tests
	
	//empty graph
	Graph g0 = new Graph(new GraphStructByAdjMat(new SquareMatrix(0))); 
	if (g0.getNumberOfConnectedComponents() == 0) 
	    nbok++; 
	nb++; 

	//graph with 3 vertices, with no edge
	Graph g1 = new Graph(new GraphStructByAdjMat(new SquareMatrix(3))); 
	if (g1.getNumberOfConnectedComponents() == 3) 
	    nbok++; 
	nb++; 

	//graph with 3 vertices, with one edge between two of the three vertices
	int [][] m2 = { {0,1,0}, 
			{1,0,0}, 
			{0,0,0} }; 
	Graph g2 = new Graph(new GraphStructByAdjMat(new SquareMatrix(m2))); 
	if (g2.getNumberOfConnectedComponents() == 2) 
	    nbok++; 
	nb++; 

	//graph with 6 vertices, 2 cc
	int [][] m3 = { {0,1,1,0,0,0}, 
			{1,0,1,0,0,0}, 
			{1,1,0,0,0,0},
			{0,0,0,0,1,0}, 
			{0,0,0,1,0,1}, 
			{0,0,0,0,1,0} }; 
	Graph g3 = new Graph(new GraphStructByAdjMat(new SquareMatrix(m3))); 
	if (g3.getNumberOfConnectedComponents() == 2) 
	    nbok++; 
	nb++; 

	//complete graph of 6 vertices
	int [][] m4 = { {0,1,1,1,1,1}, 
			{1,0,1,1,1,1},
			{1,1,0,1,1,1},
			{1,1,1,0,1,1}, 
			{1,1,1,1,0,1},
			{1,1,1,1,1,0} }; 
	Graph g4 = new Graph(new GraphStructByAdjMat(new SquareMatrix(m4))); 
	if (g4.getNumberOfConnectedComponents() == 1) 
	    nbok++; 
	nb++; 


	String res = (nb == nbok) ? "PASSED" : "FAILED"; 
	System.out.println(res); 
    }
}