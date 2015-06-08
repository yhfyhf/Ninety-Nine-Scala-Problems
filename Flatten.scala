/**
 * Created by yhf on 15/6/8.
 */

/**
 * P07
 * Flatten a nested list structure.
 */

object Flatten {
  def flatten(ls: List[Any]): List[Any] = ls.flatMap(elem => elem match {
    case elem: List[_] => flatten(elem)
    case elem          => List(elem)
  })

  def main(args: Array[String]) {
    val list = List[Any](List(1, 1), 2, List(3, List(5, 8)))
    println(flatten(list))
  }
}
