package experiments.fpPatterns.executeAroundMethodPattern;

import java.util.function.Consumer;

public class ResourceHandlingFunctional {

	public static void main(String[] args) {
		Resource4.use(resource ->
				resource.op1()
						.op2());
	}
}

class Resource4 {

	private Resource4() {
		System.out.println("created");
	}

	public Resource4 op1() {
		System.out.println("op1");
		return this;
	}

	public Resource4 op2() {
		System.out.println("op2");
		return this;
	}

	private void close() {
		System.out.println("cleanup...");
	}

	public static void use(Consumer<Resource4> block) {
		Resource4 resource = new Resource4();
		try {
			block.accept(resource);
		}
		finally {
			resource.close();
		}
	}
}
