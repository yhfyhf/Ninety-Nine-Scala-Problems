/**
 * Created by yhf on 15/6/8.
 */

/*
P02
Find the last but one element of a list.
*/

object LastButOne {

  def lastButOneBuiltin[A](ls: List[A]): A = ls.dropRight(1).last

  def lastButOneBuiltin2[A](ls: List[A]): A = ls.init.last

  // Recursive method
  def lastButOneRecursive[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => lastButOneRecursive(tail)
    case _             => throw new NoSuchElementException
  }

  def main(args: Array[String]) {
    val list = List(1, 2, 3)
    println(lastButOneBuiltin(list))
    println(lastButOneBuiltin2(list))
    println(lastButOneRecursive(list))
  }
}
