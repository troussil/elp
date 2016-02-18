import java.util.List; 
import java.util.ArrayList; 

/**
 * Class that represents a sudoku grid box 
 * as a 3x3 square indexed by its top left corner
 */
class Box extends Unit {

    /// indices of the top left position
    private int myTopLeftI, myTopLeftJ; 

    /**
     * Constructor
     * @param i row index of the top left position
     * @param j column index of the top left position
     */
    public Box(int i, int j) {
	myTopLeftI = i; 
	myTopLeftJ = j;  
    }

    /**
     * @return list of the 9 positions of the 3x3 square
     */
    public List<Position> positions() {
	List<Position> list = new ArrayList<Position>(9); 
	for (int i =  0; i < 3; i++) 
	    for (int j =  0; j < 3; j++) 
		list.add(new Position(myTopLeftI + i, myTopLeftJ + j)); 
	return list; 
    }
}