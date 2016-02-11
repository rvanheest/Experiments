package experiments.fpPatterns.executeAroundMethodPattern;

public class ResourceHandlingARM {

	public static void main(String[] args) {
		// Java 7 ARM - Automatic Resource Management
		try (Resource3 resource = new Resource3()) {
			resource.op1();
			resource.op2();
		}
		// easy to forget try!
	}
}

class Resource3 implements AutoCloseable {

	public Resource3() {
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
