/**
 * Created by yhf on 15/6/8.
 */

/**
 * P05
 * Reverse a list.
 */

object Reverse {
  // Built-in solution
  def reverseBuiltin[A](ls: List[A]): List[A] = ls.reverse

  // Recursive solution
  def reverseRecursive[A](ls: List[A]): List[A] = ls match {
    case h :: tail => reverseRecursive(tail) ::: List(h)
    case Nil       => Nil
  }

  // Tail Recursive solution
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseTR[A](ls: List[A], cur: List[A]): List[A] = ls match {
      case h :: tail => reverseTR(tail, h :: cur)
      case Nil       => cur
    }
    reverseTR(ls, Nil)
  }

  // Pure Functional
  def reverseFunctional[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()) ((cur, h) => h :: cur)

  def reverseFunctional2[A](ls: List[A]): List[A] = ls.foldRight(List[A]()) ((h, cur) => cur ::: List(h))

  def main(args: Array[String]) {
    val list = List(1, 2, 3)
    println(reverseBuiltin(list))
    println(reverseRecursive(list))
    println(reverseTailRecursive(list))
    println(reverseFunctional(list))
    println(reverseFunctional2(list))
  }
}
