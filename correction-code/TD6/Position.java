class Position {

    int rowIndex, colIndex; 

    public Position(int aRowIndex, int aColIndex) {
	rowIndex = aRowIndex; 
	colIndex = aColIndex; 
    }

    public Position(String position) {
	if (position.length() < 2) {
	    rowIndex = 0; 
	    colIndex = 0; 
	} else {
	    rowIndex = Integer.valueOf(position.substring(0,1)); 
	    colIndex = Integer.valueOf(position.substring(1,2)); 
	}
    }

    @Override
    public boolean equals(Object o) {
	Position p = (Position) o; 
	return ( (rowIndex == p.rowIndex) && (colIndex == p.colIndex) ); 
    }

    public Row getRow() {
	return new Row(rowIndex); 
    }

    public Col getCol() {
	return new Col(colIndex); 
    }

    public Box getBox() {
	return new Box((rowIndex/3)*3, (colIndex/3)*3); //NB. integer division
    }

    @Override
    public String toString() {
	return ""+rowIndex+colIndex; 
    }

}