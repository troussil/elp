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
     * return list of the 9 positions of the column
     */
    public List<Position> positions() {
	List<Position> list = new ArrayList<Position>(9); 
	for (int i =  0; i < 9; i++) 
	    list.add(new Position(i, myIndex)); 
	return list; 
    }
}