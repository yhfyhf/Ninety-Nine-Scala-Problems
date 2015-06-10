/**
 * Created by yhf on 15/6/10.
 */

/**
 * P20
 * Remove the Kth element from a list.
 * Return the list and the removed element in a Tuple. Elements are numbered from 0.
 *
 * Example:
 * scala> removeAt(1, List('a, 'b, 'c, 'd))
 * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */

object RemoveAt {
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    if (n < 0 || n >= ls.length) throw new NoSuchElementException
    else {
      val (left, right) = ls.splitAt(n)
      (left ::: right.tail, ls(n))
    }
  }

  def main(args: Array[String]) {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b))
  }
}
