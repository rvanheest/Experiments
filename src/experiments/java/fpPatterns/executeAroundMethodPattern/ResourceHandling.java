package experiments.java.fpPatterns.executeAroundMethodPattern;

public class ResourceHandling {

	public static void main(String[] args) {
		Resource resource = new Resource();
		resource.op1();
		resource.op2();
		resource.close();
		// easy to forget close!
		// exception in code => close is not called
	}
}

class Resource {

	public Resource() {
		System.out.println("created");
	}

	public void op1() {
		System.out.println("op1");
	}

	public void op2() {
		System.out.println("op2");
	}

	public void close() {
		System.out.println("cleanup...");
	}
}
