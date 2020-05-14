package codingChallenges;

import java.util.Arrays;

public class Sorting {

  public static void main(String[] args) {
    System.out.println("insertion sort");

    int[] isas = array1();
    insertionSort(isas);

    int[] isbs = array2();
    insertionSort(isbs);

    int[] iscs = array3();
    insertionSort(iscs);

    System.out.println("sorting " + Arrays.toString(array1()) + " becomes " + Arrays.toString(isas));
    System.out.println("sorting " + Arrays.toString(array2()) + " becomes " + Arrays.toString(isbs));
    System.out.println("sorting " + Arrays.toString(array3()) + " becomes " + Arrays.toString(iscs));

    System.out.println();
    System.out.println("merge sort");

    int[] msas = mergeSort(array1());
    int[] msbs = mergeSort(array2());
    int[] mscs = mergeSort(array3());

    System.out.println("merging [1, 2, 3] and [0, 4, 5] is " + Arrays.toString(merge(new int[] { 1, 2, 3 }, new int[] { 0, 4, 5 })));
    System.out.println("sorting " + Arrays.toString(array1()) + " becomes " + Arrays.toString(msas));
    System.out.println("sorting " + Arrays.toString(array2()) + " becomes " + Arrays.toString(msbs));
    System.out.println("sorting " + Arrays.toString(array3()) + " becomes " + Arrays.toString(mscs));
  }

  public static int[] array1() {
    return new int[] { 3, 4, 1, 5, 2, 0 };
  }

  public static int[] array2() {
    return new int[] { 5, 4, 3, 2, 1, 0 };
  }

  public static int[] array3() {
    return new int[] { 0, 1, 2, 3, 4, 5 };
  }

  public static void insertionSort(int[] xs) {
    for (int i = 1; i < xs.length; i++) {
      int x = xs[i];
      int j = i - 1;

      while (j >= 0 && x < xs[j]) {
        xs[j + 1] = xs[j];
        j--;
      }
      xs[j + 1] = x;
    }
  }

  public static int[] mergeSort(int[] xs) {
    if (xs.length == 1) {
      return xs;
    }
    else {
      int[] as = mergeSort(Arrays.copyOfRange(xs, 0, xs.length / 2));
      int[] bs = mergeSort(Arrays.copyOfRange(xs, xs.length / 2, xs.length));

      return merge(as, bs);
    }
  }

  private static int[] merge(int[] as, int[] bs) {
    int i = 0;
    int j = 0;

    int k = 0;
    int[] result = new int[as.length + bs.length];

    while (i < as.length && j < bs.length) {
      int a = as[i];
      int b = bs[j];

      if (a <= b) {
        result[k] = a;
        i++;
        k++;
      }
      else {
        result[k] = b;
        j++;
        k++;
      }
    }

    while (i < as.length) {
      result[k] = as[i];
      i++;
      k++;
    }

    while (j < bs.length) {
      result[k] = bs[j];
      j++;
      k++;
    }

    return result;
  }
}
