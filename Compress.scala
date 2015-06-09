/**
 * Created by yhf on 15/6/9.
 */

/**
 * P08
 * Eliminate consecutive duplicates of list elements.
 * If a list contains repeated elements they should be replaced with a single copy of the element.
 * The order of the elements should not be changed.
 */

object Compress {
  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
    case Nil       => Nil
  }

  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressTR[A](ls: List[A], cur: List[A]): List[A] = ls match {
      case h :: tail => compressTR(ls.dropWhile(_ == h), cur ::: List(h))
      case Nil       => cur
    }
    compressTR(ls, Nil)
  }

  def compressFunctional[A](ls: List[A]): List[A] = {
    ls.foldLeft(List[A]()) { (h, t) =>
      if (h.isEmpty || h.last != t) h ::: List(t)
      else h
    }
  }

  def main(args: Array[String]) {
    val list = List(1, 2, 2, 3, 3, 3, 1, 2, 4, 5)
    println(compressRecursive(list))
    println(compressTailRecursive(list))
    println(compressFunctional(list))
  }
}
