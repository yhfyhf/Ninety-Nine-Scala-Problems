/**
 * Created by yhf on 15/6/9.
 */

/**
 * P15
 * Duplicate the elements of a list a given number of times.
 *
 * Example:
 * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */

object DuplicateN {
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap {
    e => List.fill(n) (e)
  }

  def main(args: Array[String]) {
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
}
