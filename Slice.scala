/**
 * Created by yhf on 15/6/10.
 */

/**
 * P18
 * Extract a slice from a list.
 * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to
 * but not including the Kth element of the original list. Start counting the elements with 0.
 *
 * Example:
 * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */

object Slice {
  def slice[A](i: Int, j: Int, ls: List[A]): List[A] = ls.slice(i, j)

  def slice2[A](i: Int, j: Int, ls: List[A]): List[A] = ls.dropRight(ls.length - j).drop(i)

  def sliceRecursive[A](i: Int, j: Int, ls: List[A]): List[A] = (i, j, ls) match {
    case (_, _, Nil)         => Nil
    case (_, n, _) if n <= 0 => Nil                   // end at end
    case (m, n, h :: tail)   => {
      if (m <= 0) h :: sliceRecursive(0, n - 1, tail) // slice from start
      else sliceRecursive(m - 1, n - 1, tail)         // move to start
    }
  }

  def sliceTailRecursive[A](i: Int, j: Int, ls: List[A]): List[A] = {
    def sliceTR[A](k: Int, cur: List[A], res: List[A]): List[A] = {
      if      (k < i) sliceTR(k+1, cur.tail, res)
      else if (k < j) sliceTR(k+1, cur.tail, cur.head :: res)
      else            res.reverse
    }
    sliceTR(0, ls, Nil)
  }

  def main(args: Array[String]) {
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
    assert(slice2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
    assert(sliceRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
    assert(sliceTailRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))

    assert(slice(7, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List())
    assert(slice2(7, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List())
    assert(sliceRecursive(7, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List())
    assert(sliceTailRecursive(7, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List())
  }
}
