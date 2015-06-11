import scala.reflect.ClassTag
import scala.util.Random

/**
 * Created by yhf on 15/6/11.
 */

/**
 * P25
 * Generate a random permutation of the elements of a list.
 * Hint: Use the solution of problem P23.
 *
 * Example:
 * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
 * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
 */

object RandomPermute {
  import RandomSelect.randomSelect

  def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

  def randomPermute2[A: ClassTag](ls: List[A]): List[A] = {
    val rand = new Random()
    val ret = ls.toArray
    for (i <- ret.length - 1 to 1 by -1) {
      val idx = rand.nextInt(i + 1)
      val temp = ret(i)
      ret.update(i, ret(idx))
      ret.update(idx, temp)
    }
    ret.toList
  }

  def main(args: Array[String]) {
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
    println(randomPermute2(List('a, 'b, 'c, 'd, 'e, 'f)))
  }
}
