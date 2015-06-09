import Pack.pack

/**
 * Created by yhf on 15/6/9.
 */

/**
 * P11
 * Modified run-length encoding.
 * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the
 * result list. Only elements with duplicates are transferred as (N, E) terms.
 *
 * Example:
 * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */

object EncodeModified {
  def encodeModified[A](ls: List[A]): List[Any] = {
    pack(ls).map(sublist => {
     if (sublist.length == 1) sublist.head
     else (sublist.length, sublist.head)
    })
  }

  def main(args: Array[String]) {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(encodeModified(list))
  }
}
