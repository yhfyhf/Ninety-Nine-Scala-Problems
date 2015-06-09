/**
 * Created by yhf on 15/6/9.
 */

/**
 * P09
 * Pack consecutive duplicates of list elements into sublists.
 * If a list contains repeated elements they should be placed in separate sublists.
 */

object Pack {
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (sublist, tail) = ls.span(_ == ls.head)
      tail match {
        case Nil => List(sublist)
        case _   => sublist :: pack(tail)
      }
    }
  }

  def main(args: Array[String]) {
    val list = List(1, 1, 1, 2, 3, 1, 1, 2, 4, 4, 5, 6)
    println(pack(list))
  }
}
