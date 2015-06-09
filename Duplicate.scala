/**
 * Created by yhf on 15/6/9.
 */

/**
 * P14
 * Duplicate the elements of a list.
 *
 * Example:
 * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */

object Duplicate {
  def duplicate[A](ls: List[A]): List[A] = ls.flatMap(e => List(e, e))

  def main(args: Array[String]) {
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
}
