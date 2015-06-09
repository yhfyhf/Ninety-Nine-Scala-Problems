/**
 * Created by yhf on 15/6/9.
 */

/**
 * Decode a run-length encoded list.
 * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
 *
 * Example:
 * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
 * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */

object Decode {
  def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap {
    pair => (1 to pair._1).map(_ => pair._2)
  }

  // more succinct code
  def decode2[A](ls: List[(Int, A)]): List[A] = ls flatMap { e => List.fill(e._1) (e._2) }

  def main(args: Array[String]) {
    val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(decode(list))
    println(decode2(list))
  }
}
