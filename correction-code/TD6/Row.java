import java.util.List; 
import java.util.ArrayList; 

class Row extends Unit {

    private int myIndex; 

    public Row(int index) {
	myIndex = index; 
    }

    public List<Position> positions() {
	List<Position> list = new ArrayList<Position>(9); 
	for (int i =  0; i < 9; i++) 
	    list.add(new Position(myIndex, i)); 
	return list; 
    }
}