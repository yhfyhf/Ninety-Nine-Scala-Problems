/**
 * Created by yhf on 15/6/10.
 */

/**
 * P16
 * Drop every Nth element from a list.
 *
 * Example:
 * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */

object Drop {
  def drop[A](n: Int, ls: List[A]): List[A] = ls.sliding(n, n).toList.flatMap({ e =>
    if (e.length == 3) e.init
    else e
  })

  def drop2[A](n: Int, ls: List[A]): List[A] = ls.zipWithIndex.filter(e => (e._2 + 1) % n != 0).map(_._1)

  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR[A](k: Int, ls: List[A]): List[A] = (k, ls) match {
      case (_, Nil)       => Nil
      case (1, h :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(k-1, tail)
    }
    dropR(n, ls)
  }

  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropTR[A](k: Int, ls: List[A], cur: List[A]): List[A] = (k, ls) match {
      case (_, Nil) => cur
      case (1, h :: tail) => dropTR(n, tail, cur)
      case (_, h :: tail) => dropTR(k - 1, tail, cur ::: List(h))
    }
    dropTR(n, ls, List[A]())
  }

  def main(args: Array[String]) {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(drop2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(dropTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
}
