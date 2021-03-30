package gsd.farce.features.model

import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import gsd.farce.features.model.Combinations._
import gsd.farce.features.Permutations._
import scala.util.control.Breaks._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 08/07/13
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */
object DisjPatternGenerator extends App {


  val prefix = if (args.length > 1) args(1) else ""
  val suffix = if (args.length > 2) args(2) else ""

  System.err.println("starting")
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
  val featureModel = SATFeatureModel.createFromDimacsFile(args(0), prefix, suffix)
  System.err.println("creating model")

  //get distinct features after filtering out generated variables and dead and mandatory features
  val distinctFeatures = featureModel.variables.keys.toList.filter(x => !x.endsWith("_m__") && !x.startsWith(prefix + "_X") && !x.startsWith(prefix + "x")).filterNot(_ == "TRUE").filterNot(isFeatureMandatory(_)).filterNot(isFeatureDead(_))

  println("Got distinct features")
  val allCombinations = combinations(2, distinctFeatures)

  System.err.println("calculated: " + allCombinations.size)

  def isFeatureMandatory(testFeature: String) = FeatureExprFactory.createDefinedExternal(testFeature).isTautology(featureModel)

  // this feature is mandatory
  def isFeatureDead(testFeature: String) = FeatureExprFactory.createDefinedExternal(testFeature).not.isTautology(featureModel) //this feature is dead

  def checkImplication(A: String, B: String): Boolean = {

    val featureA = FeatureExprFactory.createDefinedExternal(A)
    val featureB = FeatureExprFactory.createDefinedExternal(B)

    if ((featureA.implies(featureB)).isTautology(featureModel)) {
      //A=>B holds so don't test A=>B||X in the future
      true
    } else if ((featureA.and(featureB)).isContradiction(featureModel)) {
      //A=>!B holds so don't te  println("WILL NOT TEST: " + testFeature + " implies " + firstFeature)st A=>B||X in the future (A=>B||X would only be true in this case if A=>X holds)
      true
    }

    false
  }

  for (i <- 0 to distinctFeatures.size - 1) {
    System.err.println("============")
    System.err.println("Testing feature : " + distinctFeatures(i) + ", " + i)
    val testFeature = distinctFeatures(i)

    if (!isFeatureDead(testFeature) && !isFeatureMandatory(testFeature)) {

      //if testing for A=>, then loop over all combinations not containing A
      val currentCombinations = allCombinations.filter(x => !x.contains(testFeature))

      breakable {
        var currentFirstFeature = ""
        var firstFeatureImplied = false
        for (combination <- currentCombinations) {

          //take advantage that the combinations are ordered meaning that if this is A=>B|C, the next one is likely A=>B|X
          //as long as we have the same first feature then we won't check again if it is implied
          if (currentFirstFeature != combination(0)){
            currentFirstFeature = combination(0)
            firstFeatureImplied = checkImplication(testFeature, currentFirstFeature)
          }

          //if A=>B does not hold, then test A=>B|C
          if (!firstFeatureImplied) {
            val fexprString = testFeature + " => " + combination.reduceLeft(_ + " || " + _)

            val fexpr = featureExprParser.parse(fexprString)
            if (fexpr.isTautology(featureModel)) {
              println(fexprString)
              break
            }
          }
        }
      }
    } else {
      System.err.println(testFeature + " is dead.. will not test")
    }
  }


  var done = 0

}
