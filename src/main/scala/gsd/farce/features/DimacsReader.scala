package gsd.farce.features

import org.sat4j.core.{VecInt, Vec}
import org.sat4j.specs.IVecInt
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import de.fosd.typechef.featureexpr.sat.{SATFeatureExprFactory, SATFeatureExpr}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 16/04/13
 * Time: 4:06 PM
 * To change this template use File | Settings | File Templates.
 */
class DimacsReader {


  var variables: Map[Int, String] = Map()
  var clauses = new Vec[IVecInt]()
  var varIdx = 0
  val featureExprParser = new FeatureExprParser()
  var prefix = ""
  var suffix = ""

  def main(args: Array[String]) {

    if (args.length > 1) prefix = args(1)
    if (args.length > 2) suffix = args(2)
    readDimacs(args(0))

  }

  def lookupFeature(literal: Int) = {
    val feature = FeatureExprFactory.createDefinedExternal(variables.getOrElse(literal.abs, ""))
    if (literal < 0)
      feature.not()
    else
      feature
  }


  def lookupLiteral(literal: String) =
    if (literal.startsWith("-"))
      "!" + variables.getOrElse(literal.substring(1).toInt, "")
    else
      variables.getOrElse(literal.toInt, "")

  def lookupLiteral(literal: Int) =
    if (literal < 0)
      "!" + variables.getOrElse(literal.abs, "")
    else
      variables.getOrElse(literal, "")


  def readDimacs(inputFile: String) {
    for (line <- scala.io.Source.fromFile(inputFile).getLines) {
      if (line.startsWith("c")) {
        val parts = line.split(" ")
        //ignore generated variables and _m variables
        if (!parts(1).endsWith("$")){// && !parts(2).endsWith("_m")) {
          variables = variables.updated(parts(1).toInt,  prefix + parts(2) + suffix )
        }else{
          variables = variables.updated(parts(1).substring(0, parts(1).length - 1).toInt,  prefix + parts(2) + suffix )
        }

      } else if (!line.startsWith("p")) {

        val vec = new VecInt()
        val parts = line.split(" ")
        var index = 0 //use the parts length to avoid ending in 0 vs ending in 1 problem
        while (index < parts.length - 1) {
          //if (literal != "1")
          vec.push(parts(index).toInt)
          index += 1
        }
        assert(!vec.isEmpty)
        clauses.push(vec)
      }

    }
  }



  def getFeatureExpr: FeatureExpr ={
    var featureExpr = FeatureExprFactory.True
    for (i <- 0 to (clauses.size - 1)) {
      val cl = clauses.get(i)
      val values = cl.toArray.toList.slice(0, cl.size()).map(lookupFeature)
      featureExpr = featureExpr.and(values.reduceLeft(_ or _))
    }

    featureExpr
  }

  def printClauses(){
    for (i <- 0 to (clauses.size - 1)) {
      val cl = clauses.get(i)
      val values = cl.toArray.toList.map(lookupLiteral)
      values.reduceLeft(_ + "|" + _)
    }
  }


}


