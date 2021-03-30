package gsd.farce.features.model

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 16/10/13
 * Time: 5:01 PM
 * To change this template use File | Settings | File Templates.
 */
object Combinations {

  // Source: http://aperiodic.net/phil/scala/s-99/p26.scala

  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](lst: List[A])(f: (List[A]) => List[B]): List[B] =
    lst match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }


  def choose[A](n: Int, lst: List[A]) : List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(lst){
      case head::tail => choose(n - 1, tail).map { head :: _ }
      case _ => sys error "should never happen"
    }
  }

}
