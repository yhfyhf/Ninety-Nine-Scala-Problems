/**
 * Created by yhf on 15/6/8.
 */

/*
P01
Find the last element of a list.
*/

object Last {
  // Built-in method
  def lastBuiltin[A](ls: List[A]): A = ls.last

  // Recursive method
  def lastRecursive[A](ls:List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }

  def main (args: Array[String]) {
    val list = List(1, 2, 3)
    println(lastBuiltin(list))
    println(lastRecursive(list))
  }
}
