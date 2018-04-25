public class MultiMapTest {

    public static void main(String [] args) {

	MultiMap<String, Integer> mm = new MultiMap<String, Integer>(); 
	mm.putOneValue("a",1);
	mm.putOneValue("a",2);
	mm.putOneValue("b",1);
	mm.putOneValue("c",3);
	mm.putOneValue("a",5);
	System.out.println(mm);
	System.out.println(mm.containsOneValue(3));
	System.out.println(mm.containsOneValue(4));
    }
}
