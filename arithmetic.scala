/**
 * Created by yhf on 15/6/12.
 */

package arithmetic {

  import S99Int._

  class S99Int(val num: Int) {

    /**
     * P31
     * Determine whether a given integer number is prime.
     * scala> 7.isPrime
     * res0: Boolean = true
     */
    def isPrime: Boolean = (num > 1) && (primes takeWhile { _ <= Math.sqrt(num) } forall {num % _ != 0})

    /**
     * P33
     * Determine whether two positive integer numbers are coprime.
     * Two numbers are coprime if their greatest common divisor equals 1.
     * scala> 35.isCoprimeTo(64)
     * res0: Boolean = true
     */
    def isCoprimeTo(n: Int): Boolean = gcd(num, n) == 1

    /**
     * P34
     * Calculate Euler's totient function phi(m).
     * Euler's so-called totient function phi(m) is defined as the number of
     * positive integers r (1 <= r < m) that are coprime to m.  As a special
     * case, phi(1) is defined to be 1.
     * scala> 10.totient
     * res0: Int = 4
     */
    def totient: Int = (1 to num - 1).toList.count(_.isCoprimeTo(num))

    /**
     * P35
     * Determine the prime factors of a given positive integer.
     * Construct a flat list containing the prime factors in ascending order.
     * scala> 315.primeFactors
     * res0: List[Int] = List(3, 3, 5, 7)
     */
    def primeFactors: List[Int] = {
      def primeFactorsRecursive(n: Int, cur: Stream[Int]): List[Int] = {
        if (n.isPrime) List(n)
        else if (n % cur.head == 0) cur.head :: primeFactorsRecursive(n / cur.head, cur)
        else primeFactorsRecursive(n, cur.tail)
      }
      primeFactorsRecursive(num, primes)
    }

    /**
     * P36
     * Determine the prime factors of a given positive integer (2).
     * Construct a list containing the prime factors and their multiplicity.
     * scala> 315.primeFactorMultiplicity
     * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
     * Alternately, use a Map for the result.
     * scala> 315.primeFactorMultiplicity
     * res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
     */
    def primeFactorMultiplicity: List[(Int, Int)] = num.primeFactors.groupBy(e => e).toList.map(p => (p._1, p._2.length)).sortBy(_._1)
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2).filter(_.isPrime))

    /**
     * P32
     * Determine the greatest common divisor of two positive integer numbers.
     * Use Euclid's algorithm.
     * scala> gcd(36, 63)
     * res0: Int = 9
     */
    def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

    def main(args: Array[String]) {
      assert(7.isPrime == true)
      assert(gcd(36, 63) == 9)
      assert(35.isCoprimeTo(64) == true)
      assert(10.totient == 4)
      assert(315.primeFactors == List(3, 3, 5, 7))
      assert(315.primeFactorMultiplicity == List((3,2), (5,1), (7,1)))
    }
  }
}
