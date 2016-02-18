import java.io.IOException; 

class DemoSudoku {

    public static void main(String args[]) throws IOException {

	SudokuSolver solver = new SudokuSolver(System.in); 
	solver.displayOriginalGrid(System.out); 
	solver.displayWorkingGrid(System.out);

	if (solver.solve()) 
	    System.out.println("Valid:"); 
	else
	    System.out.println("Not valid..."); 
	solver.displayWorkingGrid(System.out);    
	
    }

}