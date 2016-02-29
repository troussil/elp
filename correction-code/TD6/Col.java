import java.util.List; 
import java.util.ArrayList; 

/**
 * Class that represents a sudoku grid column
 */
class Col extends Unit {

    /// index of the column
    private int myIndex; 

    /** 
     * Constructor
     * @param index column index
     */
    public Col(int index) {
	myIndex = index; 
    }

    /**
     * return list of the 9 squares of the column
     */
    public List<Square> squares() {
	List<Square> list = new ArrayList<Square>(9); 
	for (int i =  0; i < 9; i++) 
	    list.add(new Square(i, myIndex)); 
	return list; 
    }
}