import java.util.List; 
import java.util.ArrayList; 

class Col extends Unit {

    private int myIndex; 

    public Col(int index) {
	myIndex = index; 
    }

    public List<Position> positions() {
	List<Position> list = new ArrayList<Position>(9); 
	for (int i =  0; i < 9; i++) 
	    list.add(new Position(i, myIndex)); 
	return list; 
    }
}