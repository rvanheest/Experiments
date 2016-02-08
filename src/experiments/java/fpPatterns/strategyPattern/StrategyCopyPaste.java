package experiments.java.fpPatterns.strategyPattern;

import java.util.Arrays;
import java.util.List;

public class StrategyCopyPaste {

	public static void main(String[] args) {
		List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
		
		System.out.println(totalValues(numbers));
		System.out.println(totalEvenValues(numbers));
		System.out.println(totalOddValues(numbers));
	}
	
	public static int totalValues(List<Integer> values) {
		int result = 0;
		
		for (int e : values) {
			result += e;
		}
		
		return result;
	}
	
	public static int totalEvenValues(List<Integer> values) {
		int result = 0;
		
		for (int e : values) {
			if (e % 2 == 0)
				result += e;
		}
		
		return result;
	}
	
	public static int totalOddValues(List<Integer> values) {
		int result = 0;
		
		for (int e : values) {
			if (e % 2 != 0)
				result += e;
		}
		
		return result;
	}
}
