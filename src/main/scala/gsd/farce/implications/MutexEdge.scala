package gsd.farce.implications

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/02/13
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */
class MutexEdge(feat1: String, feat2: String) {

  //used to record comparison results when comparing graphs
  var comparisonArray: Array[Int]  = null

  def initializeCompArray(size: Int) {
    comparisonArray = new Array[Int](size)
  }

  def printComparison(){
    print(toString + ",")
    comparisonArray.map(x => print(x+ ",") )
    println("")
  }

  def setComparison(index: Int){
    comparisonArray(index) = 1
  }

  def getFeat1 = feat1

  def getFeat2 = feat2

  //x--y is the same as y--x in a mutex graph
  override def equals(obj: Any) = (obj.asInstanceOf[MutexEdge].getFeat1.equals(feat1) && obj.asInstanceOf[MutexEdge].getFeat2.equals(feat2)) || (obj.asInstanceOf[MutexEdge].getFeat2.equals(feat1) && obj.asInstanceOf[MutexEdge].getFeat1.equals(feat2))


  override def toString = feat1 + " -- " + feat2


}
