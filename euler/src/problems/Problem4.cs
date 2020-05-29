using System;
using System.Linq;
using System.Collections.Generic;

namespace euler {

    class Problem4 {

        public static void LargestPalindromeProduct() {
            Console.WriteLine(
                range(100, 999).Reverse()
                    .SelectMany(a => range(100, a).Reverse().Select(b => a * b))
                    .Where(n => n.ToString().Reverse().SequenceEqual(n.ToString()))
                    .Max()
            );
        }

        private static IEnumerable<int> range(int start, int stop) {
            return Enumerable.Range(start, stop - start + 1);
        }
    }
}