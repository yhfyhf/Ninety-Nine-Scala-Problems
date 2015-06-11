import scala.util.Random

/**
 * Created by yhf on 15/6/11.
 */

/**
 * P23
 * Extract a given number of randomly selected elements from a list.
 *
 * Example:
 * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))
 * res0: List[Symbol] = List('e, 'd, 'a)
 *
 * Hint: Use the solution to problem P20
 */

object RandomSelect {

  import RemoveAt.removeAt

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
      val (sub, e) = removeAt((new Random()).nextInt(ls.length), ls)
      e :: (randomSelect(n - 1, sub))
    }
  }

  def main(args: Array[String]) {
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))
  }
}
