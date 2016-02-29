public class CubbyHole {
    protected Product myProduct;

    public Product get() {
	Product res = myProduct; 
	myProduct = null; 
	System.out.println("get " + res);
	return res;
    }

    public void put(Product aProduct) {
	myProduct = aProduct;
	System.out.println("put " + myProduct); 
    }
}

