public class Product {
    long myKey; 
    public Product(long aKey) {
	myKey = aKey; 
    }

    @Override
    public String toString() {
	return "" + myKey; 
    }
}