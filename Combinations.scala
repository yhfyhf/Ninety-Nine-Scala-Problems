/**
 * Created by yhf on 15/6/11.
 */

/**
 * P26
 * Generate the combinations of K distinct objects chosen from the N elements of a list.
 * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220
 * possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great.
 * But we want to really generate all the possibilities.
 *
 * Example:
 * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
 * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
 */

object Combinations {
  def mycombinations[A](k: Int, ls: List[A]): List[List[A]] = ls.combinations(k).toList

  def main(args: Array[String]) {
    println(mycombinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
  }
}
