/**
 * Created by yhf on 15/6/8.
 */

/*
P03
Find the Kth element of a list.
*/

object Nth {
  def nthBuiltin[A](ls: List[A], n: Int): A =
    if (n < 0 || n >= ls.length) throw new NoSuchElementException
    else ls(n)

  def nthRecursive[A](ls: List[A], n: Int): A = (ls, n) match {
    case (h :: tail, 0) => h
    case (_ :: tail, n) => nthRecursive(tail, n-1)
    case (_, _) => throw new NoSuchElementException
  }

  def main(args: Array[String]) = {
    val list = List(1, 2, 3)
    println(nthBuiltin(list, 2))
//    println(nthBuiltin(list, 3))
    println(nthRecursive(list, 2))
//    println(nthRecursive(list, 3))
  }
}
