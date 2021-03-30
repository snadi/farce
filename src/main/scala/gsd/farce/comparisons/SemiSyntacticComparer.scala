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
object SemiSyntacticComparer extends App{

 // var featureModelConstraints = Set[FeatureExpr]()
  var typeParserErrorConstraits  = Set[FeatureExpr]()
  var nestedIfDefConstraints = Set[FeatureExpr]()
  var defUseConstraints = Set[FeatureExpr]()

  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)

  println("Implication, Nested Ifdef, Type/Parser Errors, Defuse")

  for(line <- Source.fromFile("output/Type_ParserErrors/extractedErrorImpls.txt").getLines()){
    typeParserErrorConstraits += featureExprParser.parse(line)
  }

  for(line <- Source.fromFile("output/NestedIfdefs/nestedIfDefImpls.txt").getLines()){
    nestedIfDefConstraints += featureExprParser.parse(line)
  }

  for(line <- Source.fromFile("output/DefUse/defUseImplications.txt").getLines()){
    defUseConstraints += featureExprParser.parse(line)
  }

  //counting variables
  var countAll = 0
  var countFound = 0
  var countNotFound = 0
  var countErrors = 0
  var countNested = 0
  var countDefUse = 0

  for (line <- Source.fromFile(args(0)).getLines()) {
    val constraint = featureExprParser.parse(line)
    countAll += 1



    var found = false

    var outputLine = line + ","


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
      countErrors += 1
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
  System.err.println("total implications in feature model found in type/parser typesystem:  " + countErrors)
  System.err.println("total implications in feature model found in nested ifdefs: " + countNested)
  System.err.println("total implications in feature mdoel found in def/use: " + countDefUse)


}
