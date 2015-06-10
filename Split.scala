/**
 * Created by yhf on 15/6/10.
 */

/**
 * P17
 * Split a list into two parts.
 * The length of the first part is given. Use a Tuple for your result.
 *
 * Example:
 * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 */

object Split {
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

  def split2[A](n: Int, ls: List[A]): (List[A], List[A]) = (ls.take(n), ls.drop(n))

  def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil)       => (Nil, Nil)
    case (0, l)         => (Nil, l)
    case (_, h :: tail) => {
      val (left, right) = splitRecursive(n - 1, tail)
      (h :: left, right)
    }
  }

  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitTR[A](k: Int, left: List[A], right: List[A]): (List[A], List[A]) = (k, right) match {
      case (_, Nil)        => (left, right)
      case (0, _)          => (left, right)
      case (_, h :: tail)  => splitTR(k - 1, left ::: List(h), tail)
    }
    splitTR(n, List[A](), ls)
  }

  def main(args: Array[String]) {
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(split2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(splitRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(splitTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
