import java.util.List; 
import java.util.ArrayList; 

/**
 * Class that represents a sudoku grid row
 */
class Row extends Unit {

    /// index of the row
    private int myIndex; 

    /** 
     * Constructor 
     * @param index of the row
     */
    public Row(int index) {
	myIndex = index; 
    }

    /**
     * @return list of the 9 squares of the row
     */
    public List<Square> squares() {
	//TODO
    }
}
