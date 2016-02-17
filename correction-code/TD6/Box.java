import java.util.List; 
import java.util.ArrayList; 

class Box extends Unit {

    private int myTopLeftI, myTopLeftJ; 

    public Box(int i, int j) {
	myTopLeftI = i; 
	myTopLeftJ = j;  
    }

    public List<Position> positions() {
	List<Position> list = new ArrayList<Position>(9); 
	for (int i =  0; i < 3; i++) 
	    for (int j =  0; j < 3; j++) 
	    list.add(new Position(myTopLeftI + i, myTopLeftJ + j)); 
	return list; 
    }
}