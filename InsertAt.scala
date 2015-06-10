/**
 * Created by yhf on 15/6/10.
 */

/**
 * P21
 * Insert an element at a given position into a list.
 *
 * Example:
 * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
 * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */

object InsertAt {
  def insertAt[A](elem: A, index: Int, ls: List[A]): List[A] = ls.take(index) ::: List(elem) ::: ls.drop(index)

  def main(args: Array[String]) {
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }
}
