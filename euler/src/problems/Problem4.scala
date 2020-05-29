package problems

object Problem4 extends App {

  val palindromes = for {
    a <- Stream.range(999L, 100, -1)
    b <- Stream.range(a, 100, -1)
    n = a * b
    if isPalindrome(n.toString)
  } yield n

  println(palindromes.max)
  
  def isPalindrome(s: String): Boolean = {
    s.reverse == s
  }
}
