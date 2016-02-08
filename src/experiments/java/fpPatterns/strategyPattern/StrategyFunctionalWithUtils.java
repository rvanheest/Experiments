package experiments.java.fpPatterns.strategyPattern;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class StrategyFunctionalWithUtils {

	public static void main(String[] args) {
		List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

		System.out.println(totalValues(numbers, e -> true)); // add all numbers
		System.out.println(totalValues(numbers, Util::isEven)); // add even numbers
		System.out.println(totalValues(numbers, Util::isOdd)); // add odd numbers
	}

	public static int totalValues(List<Integer> values, Predicate<Integer> selector) {
		int result = 0;

		for (int e : values) {
			if (selector.test(e))
				result += e;
		}

		return result;
	}
}

class Util {

	public static boolean isEven(int number) {
		return number % 2 == 0;
	}

	public static boolean isOdd(int number) {
		return number % 2 != 0;
	}
}
