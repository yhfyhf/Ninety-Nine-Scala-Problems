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

    /**
     * P40
     * Goldbach's conjecture.
     * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
     * E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in
     * the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can
     * represent). Write a function to find the two prime numbers that sum up to a given even integer.
     * scala> 28.goldbach
     * res0: (Int, Int) = (5,23)
     */
    def goldbach: (Int, Int) = {
      primes takeWhile { _ < num / 2 + 1 } find { p => (num - p).isPrime } match {
        case None     => throw new IllegalArgumentException
        case Some(p1) => (p1, num - p1)
      }
    }
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

    /**
     * P39
     * A list of prime numbers.
     * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
     * scala> listPrimesinRange(7 to 31)
     * res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
     */
    def listPrimesRange(r: Range): List[Int] = primes.dropWhile(_ < r.head).takeWhile(_ <= r.last).toList

    /**
     * P41
     * A list of Goldbach compositions.
     * Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
     * scala> printGoldbachList(9 to 20)
     * 10 = 3 + 7
     * 12 = 5 + 7
     * 14 = 3 + 11
     * 16 = 3 + 13
     * 18 = 5 + 13
     * 20 = 3 + 17
     * In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very
     * rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
     *
     * Example (minimum value of 50 for the primes):
     * scala> printGoldbachListLimited(1 to 2000, 50)
     * 992 = 73 + 919
     * 1382 = 61 + 1321
     * 1856 = 67 + 1789
     * 1928 = 61 + 1867
     */
    def printGoldbachListLimited(r: Range): Unit = r.filter(_ % 2 == 0).map(_.goldbach).foreach(p => println(p._1+p._2+" = "+p._1+" + "+p._2))

    def printGoldbachListLimited(r: Range, min: Int): Unit = r.filter(
      p => p > 2 && p % 2 == 0
    ).map(_.goldbach).filter(_._1 > 50) foreach {
      p => println(p._1 + p._2 + " = " + p._1 + " + " + p._2)
    }

    def main(args: Array[String]) {
      assert(7.isPrime == true)
      assert(gcd(36, 63) == 9)
      assert(35.isCoprimeTo(64) == true)
      assert(10.totient == 4)
      assert(315.primeFactors == List(3, 3, 5, 7))
      assert(315.primeFactorMultiplicity == List((3,2), (5,1), (7,1)))
      assert(listPrimesRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
      assert(28.goldbach ==(5, 23))
      printGoldbachListLimited(9 to 20)
      printGoldbachListLimited(9 to 2000, 50)
    }
  }
}
