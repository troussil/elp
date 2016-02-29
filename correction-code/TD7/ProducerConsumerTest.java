public class ProducerConsumerTest {

    public static void main(String arg[]) {

        CubbyHole c = new SyncCubbyHole();
        Producer producer = new Producer(c);
        Consumer consumer = new Consumer(c);

	Thread tproducer = new Thread(producer); 
	tproducer.start(); 
	Thread tconsumer = new Thread(consumer); 
	tconsumer.start(); 

    }
}
