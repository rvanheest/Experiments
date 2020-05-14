package codingChallenges;

import java.util.stream.IntStream;

public class FibDP {

  public static void main(String[] args) {
    IntStream.rangeClosed(1, 10).map(FibDP::fib).forEach(System.out::println);
    IntStream.rangeClosed(1, 10).map(FibDP::fibMem).forEach(System.out::println);
    IntStream.rangeClosed(1, 10).map(FibDP::fibDP).forEach(System.out::println);
  }

  public static int fib(int n) {
    if (n <= 2)
      return 1;
    else
      return fib(n - 1) + fib(n - 2);
  }

  public static int fibMem(int n) {
    return fibMem(n, new int[n]);
  }

  private static int fibMem(int n, int[] mem) {
    if (mem[n - 1] != 0)
      return mem[n - 1];

    int res;
    if (n <= 2)
      res = 1;
    else
      res = fibMem(n - 1, mem) + fibMem(n - 2, mem);

    mem[n - 1] = res;
    return res;
  }

  public static int fibDP(int n) {
    if (n <= 2)
      return 1;

    int[] mem = new int[n];
    mem[0] = 1;
    mem[1] = 1;

    for (int i = 2; i < n; i++) {
      mem[i] = mem[i - 1] + mem[i - 2];
    }

    return mem[n - 1];
  }
}
