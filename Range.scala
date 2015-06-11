/**
 * Created by yhf on 15/6/11.
 */

/**
 * P22
 * Create a list containing all integers within a given range.
 *
 * Example:
 * scala> range(4, 9)
 * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */

object Range {
  def range(i: Int, j: Int): List[Int] = (i to j).toList

  def range2(i: Int, j: Int): List[Int] = List.range(i, j + 1)

  def rangeRecursive(i: Int, j: Int): List[Int] = {
    def rangeR(k: Int): List[Int] = {
      if (k <= j) k :: rangeR(k + 1)
      else Nil
    }
    rangeR(i)
  }

  def rangeTailRecursive(i: Int, j: Int): List[Int] = {
    def rangeTR(k: Int, cur: List[Int]): List[Int] = {
      if (k <= j) rangeTR(k + 1, k :: cur)
      else cur.reverse
    }
    rangeTR(i, Nil)
  }

  def main(args: Array[String]) {
    assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
    assert(range2(4,9)== List(4, 5, 6, 7, 8, 9))
    assert(rangeRecursive(4, 9) == List(4, 5, 6, 7, 8, 9))
    assert(rangeTailRecursive(4, 9) == List(4, 5, 6, 7, 8, 9))
  }
}
