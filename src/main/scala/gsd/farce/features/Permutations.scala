package gsd.farce.features

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 21/10/13
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates.
 */
object Permutations {

  def nPk[A](list: List[A], k: Int) = {
    val result = scala.collection.mutable.Set[List[A]]()
    val iter = list.permutations
    val n = list.length
    while (iter.hasNext) {
      val next = iter.next
      result += next.dropRight(n - k)
    }
    result
  }
}
