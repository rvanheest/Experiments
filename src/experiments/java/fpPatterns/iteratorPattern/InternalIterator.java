package experiments.java.fpPatterns.iteratorPattern;

import java.util.Arrays;
import java.util.List;

public class InternalIterator {

	public static void main(String[] args) {
		List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
		
		numbers.forEach(System.out::println);
	}
}
