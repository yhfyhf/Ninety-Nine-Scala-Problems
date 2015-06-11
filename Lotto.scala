/**
 * Created by yhf on 15/6/11.
 */

/**
 * P24
 * Lotto: Draw N different random numbers from the set 1..M.
 *
 * Example:
 * scala> lotto(6, 49)
 * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */

object Lotto {
  import RandomSelect.randomSelect
  def lotto(n: Int, m: Int): List[Int] = randomSelect(n, (1 to m).toList)

  def main(args: Array[String]) {
    println(lotto(6, 49))
  }
}
