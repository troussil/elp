/**
 * Class that represents a position of a sudoku grid as a
 * pair of integer coordinates in 0..8
 */
class Position {

    /// coordinates
    int rowIndex, colIndex; 

    /// upper bound of the coordinate range (excluded)
    private static int upperBound = 9; 

    /**
     * Constructor
     * @param aRowIndex index of the row
     * @param aColIndex index of the column
     */
    public Position(int aRowIndex, int aColIndex) {
	rowIndex = shift(aRowIndex); 
	colIndex = shift(aColIndex); 
    }

    /**
     * Constructor
     * @param position string representation of a position
     * @warning if position has 0 or 1 character, both index 
     * are set to 0; if position has more than 2 characters, 
     * extra characters are ignored
     */
    public Position(String position) {
	if (position.length() < 2) {
	    rowIndex = 0; 
	    colIndex = 0; 
	} else {
	    rowIndex = shift(Integer.valueOf(position.substring(0,1))); 
	    colIndex = shift(Integer.valueOf(position.substring(1,2))); 
	}
    }

    /**
     * Shift a given integer by modulo operations, 
     * so that it lies in 0..8
     * @param x any integer
     * @return an integer in 0..8
     */
    private int shift(int x) {
	if (x >= upperBound) 
	    return (x%upperBound); 
	else if (x < 0)
	    return ((-x)%upperBound);
	else
	    return x; 
    }

    /**
     * Checks if the current position is equal to another one 
     * @param o other position
     * @return 'true' if equal, 'false' otherwise
     */
    @Override
    public boolean equals(Object o) {
	Position p = (Position) o; 
	return ( (rowIndex == p.rowIndex) && (colIndex == p.colIndex) ); 
    }

    /**
     * Get the row the current position belongs to
     * @return row of same row index
     */
    public Row getRow() {
	return new Row(rowIndex); 
    }

    /**
     * Get the column the current position belongs to
     * @return column of same column index
     */
    public Col getCol() {
	return new Col(colIndex); 
    }
    /**
     * Get the box the current position belongs to
     * @return box
     */
    public Box getBox() {
	//we compute the coordinates of the top left position 
	//of the box by integer division
	return new Box((rowIndex/3)*3, (colIndex/3)*3); 
    }

    /**
     * Get the string representation of the current position
     * (concatenation of the two index digit)
     * @return position coordinates as a string
     */
    @Override
    public String toString() {
	return ""+rowIndex+colIndex; 
    }

}