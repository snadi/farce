package gsd.farce.comparisons

import io.Source
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 19/03/13
 * Time: 8:26 AM
 * To change this template use File | Settings | File Templates.
 */

/*
Compares constraints in feature model to those in the code syntactically using string operations
oneOf constraints in the feature model are first translated into several constraints OneOf(A,B) translates to
A=>!B, B=>!A
 */
object SyntacticComparer extends App{

  var featureModelConstraints = Set[String]()
  var typeParserErrorConstraits  = Set[String]()
  var nestedIfDefConstraints = Set[String]()
  var defUseConstraints = Set[String]()
  var errorConstraints = Set[String]()

  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)

  println("Implication, Nested Ifdef, Type/Parser Errors, Defuse")
  for(line <- Source.fromFile("gitbusybox/featureModel").getLines()){
    if(line.startsWith("oneOf")){
      val fexpr = featureExprParser.parse(line)
      val distinctFeatures = fexpr.collectDistinctFeatureObjects.asInstanceOf[Set[FeatureExpr]]
      var fexpr2 = FeatureExprFactory.True

      for (fexpr1 <- distinctFeatures){
        val remainingSet = distinctFeatures - fexpr1
        fexpr2 = remainingSet.reduceLeft(_.not() and _.not())
        featureModelConstraints += (fexpr1 + " => " + fexpr2).trim()
      }
    } else{
      featureModelConstraints += line.replaceAllLiterally("defined", "def").trim()
    }
  }

  typeParserErrorConstraits = Source.fromFile("output/Type_ParserErrors/extractedErrorImpls.txt").getLines().toSet[String]

  nestedIfDefConstraints = Source.fromFile("output/NestedIfdefs/nestedIfDefCombinedImplications.txt").getLines().toSet[String]

  defUseConstraints= Source.fromFile("output/DefUse/defUseImplications.txt").getLines().toSet[String]

  errorConstraints= Source.fromFile("output/Errors/allErrors.txt").getLines().toSet[String]

  //counting variables
  var countAll = 0
  var countFound = 0
  var countNotFound = 0
  var countType = 0
  var countNested = 0
  var countDefUse = 0
  var countErrors = 0

  for (constraint <- featureModelConstraints) {
    countAll += 1

    var found = false

    var outputLine = constraint + ","


    //test for implication in each of the three models and alter output line accordingly
    if (nestedIfDefConstraints.contains(constraint)) {
      found = true
      countNested += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    if (typeParserErrorConstraits.contains(constraint)) {
      found = true
      countType += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    if (defUseConstraints.contains(constraint)) {
      found = true
      countDefUse += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    if (errorConstraints.contains(constraint)) {
      found = true
      countErrors += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    //update counts
    if (!found) {
      countNotFound += 1
      System.err.println("NOT FOUND: " + constraint)
    } else {
      countFound += 1
    }

    println(outputLine)
  }

  System.err.println("total implications in feature model: " + countAll)
  System.err.println("total implications in feature model found in code: " + countFound)
  System.err.println("total implications in feature model not found in code: " + countNotFound)
  System.err.println("total implications in feature model found in type/parser typesystem:  " + countType)
  System.err.println("total implications in feature model found in nested ifdefs: " + countNested)
  System.err.println("total implications in feature mdoel found in def/use: " + countDefUse)
  System.err.println("total implications in feature mdoel found in errors: " + countErrors)


}
