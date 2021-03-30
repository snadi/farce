package gsd.farce.implications

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/02/13
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */
class Implication(feat1: String, feat2: String) {

  //used to record comparison results when comparing graphs
  var comparisonArray: Array[Int] = null

  def initializeCompArray(size: Int) {
    comparisonArray = new Array[Int](size)
  }

  def printComparison() {
    print(toString + ",")
    comparisonArray.map(x => print(x + ","))
    println("")
  }

  def setComparison(index: Int) {
    comparisonArray(index) = 1
  }

  def getFeat1 = feat1

  def getFeat2 = feat2

  override def equals(obj: Any) = obj.asInstanceOf[Implication].getFeat1.equals(feat1) && obj.asInstanceOf[Implication].getFeat2.equals(feat2)

  override def hashCode: Int = {
    var hash = 7;


    val feat1_code = {
      if (feat1 == null)
        0
      else
        feat1.hashCode
    }

    hash = 31*hash + feat1_code

    val feat2_code = {
      if (feat2 == null)
        0
      else
        feat2.hashCode
    }

    hash = 31*hash + feat2_code

    hash
  }


  override def toString = feat1 + " => " + feat2


}
