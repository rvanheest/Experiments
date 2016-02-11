package experiments.fpPatterns.strategyPattern;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class StrategyOneFunction {

	public static void main(String[] args) {
		List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
		
		System.out.println(totalValues(numbers, e -> true)); // add all numbers
		System.out.println(totalValues(numbers, e -> e % 2 == 0)); // add even numbers
		System.out.println(totalValues(numbers, e -> e % 2 != 0)); // add odd numbers
	}

	public static int totalValues(List<Integer> values, Predicate<Integer> selector) {
		return values.stream()
				.filter(selector)
				.reduce(0, Integer::sum);
	}
}
