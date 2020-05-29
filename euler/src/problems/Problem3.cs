using System;
using System.Linq;
using System.Collections.Generic;

namespace euler {

    class Problem3 {
        public static void LargestPrimeFactor(long n) {
            Console.WriteLine(primes(n, 2, new HashSet<long>()).Max());
        }

        private static ISet<long> primes(long n, long p, ISet<long> found) {
            if (n < p * p) {
                found.Add(n);
                return found;
            }
            else if (n % p == 0) {
                found.Add(p);
                return primes(n / p, p, found);
            }
            else {
                return primes(n, p + 1, found);
            }
        }
    }
}