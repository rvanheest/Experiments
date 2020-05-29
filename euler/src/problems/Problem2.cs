using System;
using System.Linq;

namespace euler {
    class Problem2 {
        public static void SumOfEvenFibs() {
            Enumerable.Repeat(1, int.MaxValue)
                .Aggregate(Tuple.Create(0, 1), (Tup, n) => Tuple.Create(Tup.Item2, Tup.Item1 + Tup.Item2));
        }
    }
}