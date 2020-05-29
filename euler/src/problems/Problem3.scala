package problems

import scala.annotation.tailrec
import scala.collection.mutable

object Problem3 extends App {

  println(primeFactors(600851475143L))
  println(primeFactors(600851475143L).max)

  def primeFactors(n: Long): Set[Long] = {
    @tailrec
    def primes(n: Long, p: Long = 2L, found: mutable.Builder[Long, Set[Long]] = Set.newBuilder[Long]): Set[Long] = {
      if (n < p * p)
        (found += n).result()
      else if (n % p == 0)
        primes(n / p, p, found += p)
      else
        primes(n, p + 1L, found)
    }
    
    primes(n)
  }
}
