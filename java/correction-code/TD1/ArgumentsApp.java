class ArgumentsApp {
    public static void main (String[] args) {
	if (args.length > 0) {
	    System.out.println("Args: ");
	    for (int i = 0; i < args.length; i++) {
		System.out.print(args[i] + " ");
	    }
	    System.out.println();
	} else {
	    System.err.println("No args found"); 
	}
    }
}
