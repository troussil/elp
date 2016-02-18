import java.io.*; 
import java.util.*; 

class SudokuSolver {
    
    private static int size = 9;
 
    /// map of pairs (position string, digit), ie original grid to complete
    private Map<String,Integer> originalGrid = new HashMap<String,Integer>(); 
    /// map of pairs (position string, set of possible digits)
    private Map<String,DigitSet> workingGrid = new HashMap<String,DigitSet>(); 
    /// map of pairs (position string, list of position friends)
    private Map<String,List<Position> > friends = new HashMap<String,List<Position> >(); 
    /// assignation marks as a set of position 
    private Set<Position> marks = new HashSet<Position>(); 

    public SudokuSolver() throws IOException {
	this(System.in); 
    }

    public SudokuSolver(InputStream stream) throws IOException {
	fillGrid(stream); 
	fillExtraDataSructures(); 
    }

    private void fillGrid(InputStream stream) throws IOException {

	Reader reader = new InputStreamReader(stream);
	char c; //read character from reader
	int d; //read digit from c

	for(int i = 0; i < size; i++) {
	    int j = 0;
	    while (j < size) {
		c = (char) reader.read(); 
		if (Character.isDigit(c)) {
		    d = Integer.valueOf(String.valueOf(c)); 
		    if ( (d >= 1)&&(d <= 9) )
			originalGrid.put(""+i+j, d);
		    j++;
		} 
	    }
	}
    }

    private void fillExtraDataSructures() {

	/// fill friends
	for (int i =  0; i < size; i++) {
	    for (int j =  0; j < size; j++) {
		Position p = new Position(i,j); 
		List<Position> list = p.getRow().positions(); 
		list.remove(p); 
		list.addAll(p.getCol().positions()); 
		list.remove(p); 
		list.addAll(p.getBox().positions()); 
		list.remove(p); 
		friends.put(p.toString(), list); 
	    }
	}
 
	/// fill digits
	for (int i =  0; i < size; i++) {
	    for (int j =  0; j < size; j++) {
		Position p = new Position(i,j); 
		workingGrid.put(p.toString(), new DigitSet());
	    }
	}

    }

    /** Assign a digit to a position and propagate this new constraint
     * @param aMap map of pairs (String, digit set) to compute the solution
     * @param aPosition position at which we assign a digit
     * @param aDigit digit to assign
     * @return 'false' if the solution is not valid, 'true' otherwise
     */
    private boolean assign(Map<String,DigitSet> aMap, Position aPosition, int aDigit) {
	//assign,
	aMap.put(aPosition.toString(), new DigitSet(aDigit));
	//mark,
	marks.add(aPosition); 
	//and propagate
	return propagate(aMap, aPosition, aDigit);
    }

    /** Propagate a constraint from a given position where there is one possible digit
     * to all friend positions
     * @param aMap map of pairs (String, digit set) to compute the solution
     * @param aPosition position with a known digit
     * @param aDigit digit of the position 
     * @return 'false' if the solution is not valid, 'true' otherwise
     */
    private boolean propagate(Map<String,DigitSet> aMap, Position aPosition, int aDigit) {
	boolean isOK = true; 
	Iterator<Position> it = friends.get(aPosition.toString()).iterator();
	while ( (it.hasNext())&&(isOK) ) {
	    isOK = isOK && remove(aMap, it.next(), aDigit); 
	}
	return isOK; 
    }

    /** Remove a given digit from the digit set at a given position. 
     * Then, propagate this new constraint if needed, and check units
     * the position belongs to.
     * @param aMap map of pairs (String, digit set) to compute the solution
     * @param aPosition position where a digit must be removed
     * @param aDigit digit to remove 
     * @return 'false' if the solution is not valid, 'true' otherwise
     */
    public boolean remove(Map<String,DigitSet> aMap, Position aPosition, int aDigit) {

	//we remove aDigit from the digit set s at position aPosition
	DigitSet s = aMap.get(aPosition.toString()); 
	s.remove(aDigit); 

	if (s.size() == 0)
	    return false; //contradiction: only one possible digit at two positions in the same unit
	// (1) if s is reduced to one value, assign it if not already done
	else if ( (s.size() == 1)&&(!marks.contains(aPosition)) ) {
	    if (!assign(aMap, aPosition, s.firstDigit())) 
		return false;
	}
	// (2) check all units aPosition belongs to.
	boolean isOK = true; 
	isOK = isOK && checkUnit(aMap, aPosition.getRow());
	isOK = isOK && checkUnit(aMap, aPosition.getCol());
	isOK = isOK && checkUnit(aMap, aPosition.getBox());
	return isOK; 
    }

    /** Check if there is one position in a unit 
     * for which there is a possible digit, not possible
     * for other unit positions. If yes, assign this digit
     * to this position
     * @param aMap map of pairs (String, digit set) to compute the solution
     * @param aUnit any unit
     * @return 'false' if the sudoku is not valid, 'true' otherwise
     */
    private boolean checkUnit(Map<String,DigitSet> aMap, Unit aUnit) {
	boolean isOK = true; 
	//for all unit position
	Iterator<Position> it = aUnit.positions().iterator();
	while ( (it.hasNext())&&(isOK) ) {
	    Position position = it.next();
	    DigitSet s = new DigitSet( aMap.get(position.toString()) ); 
	    if (s.size() > 1) {
		//only keep digits that are not possible for other positions
		for(Position other: aUnit.positions()) {
		    if (!position.equals(other))
			s.remove( aMap.get(other.toString()) ); 
		}
		if ( (s.size() == 1)&&(!marks.contains(position)) ) 
		    //if one such digit, assign it if not already done 
		    isOK = isOK && assign(aMap, position, s.firstDigit()); 	
	    }		
	}
	return isOK; 
    }

    /** Solve the sudoku grid 
     *	@return 'false' if not valid, 'true' otherwise
     */ 
    public boolean solve() {
	/// propagate constraint from every known digit
	boolean isOK = true; 
	Iterator<Map.Entry<String,Integer> > it = originalGrid.entrySet().iterator();
	while ( (it.hasNext())&&(isOK) ) {
	    Map.Entry<String,Integer> e = it.next(); 
	    isOK = isOK && assign(workingGrid, new Position(e.getKey()), e.getValue()); 
	}
	return isOK; 
    }

    public void displayOriginalGrid(OutputStream stream) {
	PrintWriter out = new PrintWriter( new OutputStreamWriter(stream), true );
	String line; //line to print 
	Integer d; //digit to print

	out.println("Grid:"); 
	out.println(separator(1)); 
	for(int i = 0; i < size; i++) {
	    line = ""; 
	    for(int j = 0; j < size; j++) {
		if ((d = originalGrid.get(""+i+j)) != null)
		    line += " "+d+" ";
		else 
		    line += " . "; 
		if ((j % 3) == 2)
		    line += "|"; 
	    }
	    out.println(line); 
	    if ((i % 3) == 2)
		out.println(separator(1)); 
	}
    }

    public void displayWorkingGrid(OutputStream stream) {
	PrintWriter out = new PrintWriter( new OutputStreamWriter(stream), true );
	String line; 
	DigitSet s; 

	out.println("Data structure:"); 
	out.println(separator(size)); 
	for(int i = 0; i < size; i++) {
	    line = ""; 
	    for(int j = 0; j < size; j++) {
		s = workingGrid.get(""+i+j); 
		line += " "+s.toString();
		line += repeatedString(" ", size+1 - s.size()); 
		if ((j % 3) == 2)
		    line += "|"; 
	    }
	    out.println(line); 
	    if ((i % 3) == 2)
		out.println(separator(size)); 
	}
    }

    private String repeatedString(String substring, int nb) {
	String res = ""; 
	for (int k = 0; k < nb; k++)
	    res += substring; 
	return res; 
    }

    private String separator(int length) {
	String line = "";
	for (int k = 0; k < 3; k++) {
	    line += repeatedString("-", length); 
	    line += "-+";  
	    line += repeatedString("-", length); 
	    line += "-+";  
	    line += repeatedString("-", length);
	    line += "--|"; 
	}
	return line; 
    }

}