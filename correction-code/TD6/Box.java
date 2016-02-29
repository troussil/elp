import java.util.List; 
import java.util.ArrayList; 

/**
 * Class that represents a sudoku grid box 
 * as a 3x3 region indexed by its top left corner
 */
class Box extends Unit {

    /// indices of the top left square
    private int myTopLeftI, myTopLeftJ; 

    /**
     * Constructor
     * @param i row index of the top left square
     * @param j column index of the top left square
     */
    public Box(int i, int j) {
	myTopLeftI = i; 
	myTopLeftJ = j;  
    }

    /**
     * @return list of the 9 squares of the 3x3 region
     */
    public List<Square> squares() {
	List<Square> list = new ArrayList<Square>(9); 
	for (int i =  0; i < 3; i++) 
	    for (int j =  0; j < 3; j++) 
		list.add(new Square(myTopLeftI + i, myTopLeftJ + j)); 
	return list; 
    }
}