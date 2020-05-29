using System;
using System.Linq;

namespace euler {
    class Problem1 {
        public static void SumOfMultiples() {
            Console.WriteLine(Enumerable.Range(0, 1000).Where((i) => i % 3 == 0 || i % 5 == 0).Sum());
        }
    }
}