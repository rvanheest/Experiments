package experiments.java.fpPatterns.executeAroundMethodPattern;

public class ResourceHandlingExceptionSolved {

	public static void main(String[] args) {
		Resource2 resource = new Resource2();
		try {
			resource.op1();
			resource.op2();
		}
		finally {
			resource.close();
		}
		// easy to forget try, finally and close!
		// verbose!
	}
}

class Resource2 {

	public Resource2() {
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
