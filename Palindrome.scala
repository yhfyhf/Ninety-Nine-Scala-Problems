/**
 * Created by yhf on 15/6/8.
 */

/**
 * P06
 * Find out whether a list is a palindrome.
 */
object Palindrome {
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  def main(args: Array[String]) = println(isPalindrome(List(1, 2, 3, 2, 1)))
}
