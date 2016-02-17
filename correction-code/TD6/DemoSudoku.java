import java.io.IOException; 

class DemoSudoku {

    public static void main(String args[]) throws IOException {

	SudokuSolver solver = new SudokuSolver(System.in); 
	solver.displayGrid(System.out); 
	solver.displaySolution(System.out);

	if (solver.solve()) 
	    System.out.println("Valid:"); 
	else
	    System.out.println("Not valid..."); 
	solver.displaySolution(System.out);    
	
    }

}