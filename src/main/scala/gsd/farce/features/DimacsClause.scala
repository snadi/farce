package gsd.farce.features

import org.sat4j.core.VecInt

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 01/09/13
 * Time: 4:37 AM
 * To change this template use File | Settings | File Templates.
 */
class DimacsClause {

 var variables = Set[String]()

  def addVariable(variable: String) = variables += variable

  override def toString = {
    var clause = ""
    variables.foreach(x => clause += x + " ")
    clause += "0"

    clause
  }

  def getIVec:VecInt ={
    val vec = new VecInt()
    for(variable <- variables){
      vec.push(variable.toInt)
    }

    vec
  }


}
