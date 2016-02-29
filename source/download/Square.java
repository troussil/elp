/**
 * Class that represents a square of a sudoku grid as a
 * pair of integer coordinates in 0..8
 */
final class Square {

    /// coordinates
    final int rowIndex, colIndex; 

    /// upper bound of the coordinate range (excluded)
    private static final int upperBound = 9; 

    /**
     * Constructor
     * @param aRowIndex index of the row
     * @param aColIndex index of the column
     */
    public Square(int aRowIndex, int aColIndex) {
	rowIndex = shift(aRowIndex); 
	colIndex = shift(aColIndex); 
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
     * Checks if the current square is equal to another one 
     * @param o other square
     * @return 'true' if equal, 'false' otherwise
     */
    @Override
    public boolean equals(Object o) {
	if (o == null)
	    return false; 
	else {
	    Square p = (Square) o; 
	    return ( (rowIndex == p.rowIndex) && (colIndex == p.colIndex) );
	} 
    }

    /**
     * Returns the hash code of the current square  
     * @return hash code as rowIndex * 10 + colIndex
     */
    @Override
    public int hashCode() {
	return (rowIndex * 10) + colIndex;  
    }

    /**
     * Get the row the current square belongs to
     * @return row of same row index
     */
    public Row getRow() {
	return new Row(rowIndex); 
    }

    /**
     * Get the column the current square belongs to
     * @return column of same column index
     */
    public Col getCol() {
	return new Col(colIndex); 
    }
    /**
     * Get the box the current square belongs to
     * @return box
     */
    public Box getBox() {
	//we compute the coordinates of the top left square 
	//of the box by integer division
	return new Box((rowIndex/3)*3, (colIndex/3)*3); 
    }

    /**
     * Get the string representation of the current square
     * (concatenation of the two index digit)
     * @return square coordinates as a string
     */
    @Override
    public String toString() {
	return ""+rowIndex+colIndex; 
    }

}