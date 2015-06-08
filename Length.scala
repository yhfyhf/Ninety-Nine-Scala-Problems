/**
 * Created by yhf on 15/6/8.
 */

/*
P04
Find the number of elements of a list.
*/

object Length {
  // Built-in solution
  def lengthBuiltin[A](ls: List[A]): Int = ls.length

  // Recursive solution
  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case _ :: tail => 1 + lengthRecursive(tail)
    case Nil         => 0
  }

  // Tail Recursive solution
  def lengthTailRecursive[A](ls: List[A]): Int = {
    def lengthTR[A](ls: List[A], n: Int): Int = ls match {
      case _ :: tail => lengthTR(tail, n+1)
      case Nil       => n
    }
    lengthTR(ls, 0)
  }

  def main(args: Array[String]) {
    val list = List(1, 2, 3)
    println(lengthBuiltin(list))
    println(lengthRecursive(list))
    println(lengthTailRecursive(list))
  }
}
