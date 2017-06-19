package codingChallenges;

import java.util.Arrays;

public class MergeArrays {

  public static void main(String[] args) {
    System.out.println("Example 1");
    mergeArraysExample1();

    System.out.println("\nExample 2");
    mergeArraysExample2();
  }

  public static void mergeArrays(int[] as, int[] bs, int n, int m) {
    int k = n + m - 1;
    int i = n - 1;
    int j = m - 1;

    while (i >= 0 && j >= 0) {
      int a = as[i];
      int b = bs[j];

      if (a > b) {
        as[k] = a;
        i--;
      }
      else {
        as[k] = b;
        j--;
      }
      k--;
    }

    while (j >= 0) {
      as[k] = bs[j];
      j--;
      k--;
    }
  }

  public static void mergeArraysExample1() {
    int[] as = {1, 5, 9, 0, 0, 0, 0};
    int[] bs = {0, 2};
    int n = 3;
    int m = 2;

    System.out.println("merge " + Arrays.toString(as) + " with " + Arrays.toString(bs));

    mergeArrays(as, bs, n, m);
    System.out.println(Arrays.toString(as));
  }

  public static void mergeArraysExample2() {
    int[] as = {0, 2, 4, 0, 0, 0, 0};
    int[] bs = {6, 7};
    int n = 3;
    int m = 2;

    System.out.println("merge " + Arrays.toString(as) + " with " + Arrays.toString(bs));

    mergeArrays(as, bs, n, m);
    System.out.println(Arrays.toString(as));
  }
}
