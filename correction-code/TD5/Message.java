import java.util.Date;
import java.io.Serializable; 
 
class Message implements Serializable {

    private Date myDate; 
    private String myAuthor, myMsg; 

    public Message(Date aDate, String aAuthor, String aMsg) {
	myDate = aDate; 
	myAuthor = aAuthor; 
	myMsg = aMsg; 
    }

    @Override public String toString() {
	return myAuthor + ", " + myDate + ": " + myMsg; 
    }
}