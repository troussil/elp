import java.util.List; 

/**
 * Class that represents a sudoku grid unit (row, column or box)
 */
abstract class Unit {

    /**
     * @return list of the 9 squares of the unit
     */
    public abstract List<Square> squares(); 

}