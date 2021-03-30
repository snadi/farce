package gsd.farce.features.model

import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import gsd.farce.features.model.Combinations._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 08/07/13
 * Time: 5:24 PM
 * To change this template use File | Settings | File Templates.
 */
object ConjPatternGenerator extends App {


 val prefix = if (args.length > 1) args(1) else ""
  val suffix = if (args.length > 2 ) args(2) else ""

  val featureModel = SATFeatureModel.createFromDimacsFile(args(0), prefix, suffix)
  val distinctFeatures = featureModel.variables.keys.toList.filter(x => !x.endsWith("_m__") && !x.startsWith(prefix +"_X") && !x.startsWith(prefix +"x"))
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)

  for(i <- 0 to distinctFeatures.size - 1){
    val testFeature = FeatureExprFactory.createDefinedExternal(distinctFeatures(i))
    val featureMandatory = testFeature.isTautology(featureModel) // this feature is mandatory
    val featureDead = !featureMandatory && testFeature.not.isTautology(featureModel) //this feature is dead

    if (!featureDead && !featureMandatory ) {
    val remainingFeautres = List(distinctFeatures.slice(0, i), distinctFeatures.slice(i+1, distinctFeatures.size)).flatten
    var nonImpliedFeatures = Set[String]()
      val currentCombinations = combinations(2, remainingFeautres)
      System.err.println("got combinations for : "+ testFeature)
      for(list <- currentCombinations) {
         var test = true
         System.err.println("================testing combination: " +   list)
        //try to do some smart filtering for future comparisons
        //if we are testing for A => B & C, and A=>B does not hold then we don't have to test A => B & C
        for (feature <- list){
          System.err.println("testing : " + feature)
          if (nonImpliedFeatures.contains(feature)) {
            test = false
            System.err.println(feature + "has been added to list before")
          } else if (!(testFeature.implies(FeatureExprFactory.createDefinedExternal(feature))).isTautology(featureModel)) {
            System.err.println("adding " +feature + "to list")
            nonImpliedFeatures += feature
            test = false
          } else{
            System.err.println("going to test it!")
          }
        }

        //if both feature1 or feature2 are not implied seperately by the test feature
          if (test) {
            val fexprString = testFeature + " => " + list.reduceLeft(_ + " && " + _)
            System.err.println("TESTING " + fexprString)
        val fexpr = featureExprParser.parse(fexprString)
        if (fexpr.isTautology(featureModel)){
              System.err.println("PRINT LINE")
          println(fexprString)
        }else{
              System.err.println("DOESN'T HOLD")
          }
          } else {
            System.err.println("test is false")
          }
        }
    } else {
      System.err.println(testFeature + " is mandatory.. will not test")
    }
  }


  var done = 0

}
