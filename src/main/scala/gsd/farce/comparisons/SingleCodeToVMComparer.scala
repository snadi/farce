package gsd.farce.comparisons

import io.Source
import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.util.Properties
import java.io.{FileWriter, PrintWriter, FileInputStream, File}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.Utilities

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 14/03/13
 * Time: 10:14 AM
 * To change this template use File | Settings | File Templates.
 */
object SingleCodeToVMComparer extends App {

  val properties = new Properties()
  val system = args(0)
  loadPropertiesFile(system, properties)
  val config = getSystemConfig(args(0))



  val featureModel = if (properties.getProperty(FEATURE_MODEL_FILE).contains(".dimacs"))
      SATFeatureModel.createFromDimacsFile(properties.getProperty(FEATURE_MODEL_FILE), config.getPrefix, config.getSuffix)
    else {
      val fexpr = new FeatureExprParser(FeatureExprFactory.sat).parseFile(properties.getProperty(FEATURE_MODEL_FILE)).asInstanceOf[SATFeatureExpr]
      SATFeatureModel.create(fexpr).asInstanceOf[SATFeatureModel]
  }


  val featureModelFeatures = featureModel.variables.keys.toSet
  val outputDir = args(1)


  //compare the different code constraits to the variability model
  //get total accuracy of system
  var globalFound = 0
  var globalTotal = 0

  //spec1
  var currentSpecFound  = 0
  var currentSpecTotal = 0
  var constraintType = args(2).substring(args(2).lastIndexOf("/") + 1)
  constraintType = constraintType.substring(0, constraintType.indexOf("."))
  val compareIndividual = if(args.length > 3) args(3).toBoolean else false

  if(compareIndividual)
    compareConstraintList(args(2), constraintType)
  else
    compareConstraints(args(2), constraintType)


  def compareConstraints(file: String, constraintType: String) {

    System.err.println("Comparing: "+ constraintType)
    val comparisonWriter = new PrintWriter(new FileWriter(outputDir + system + "_"+ constraintType + "_accuracy.csv"))
    var countFound = 0
    var countNotFound = 0
    var constraintsRead = Set[FeatureExpr]()
    comparisonWriter.println("Code Constraint, Found")
    var lineCount = 1
    try{
    for (line <- Source.fromFile(file).getLines()) {
      if (line.length > 0) {
        System.err.println("Reading " + constraintType +" line: " + lineCount)
        lineCount+=1
        val input = new ANTLRInputStream(line.trim())
        val lexer = new FExprLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new FExprParser(tokens)
        val testExpr = parser.fexpr().value

        if (testExpr == null) {
          System.err.println("null from line:" + line)
        }
        if (!constraintsRead.contains(testExpr)) {
          constraintsRead += testExpr
          var found = false

          val testFeatures = testExpr.collectDistinctFeatures

          var outputLine = line + ","

          if (testExpr.isContradiction()) {
            System.err.println(testExpr + "ITS A CONTRADITION!!")
          }

          if (testExpr.isTautology()) {
            System.err.println("LINE: " + line)
            System.err.println(testExpr + "ITS A TAUTOLOGY!!")
          }

          //testExpr should be a tautology in feature model
          if (testFeatures.subsetOf(featureModelFeatures) && (testExpr).isTautology(featureModel)) {
            found = true
            countFound += 1
            outputLine += "1"
          } else {
            outputLine += "0"
            countNotFound += 1
          }
          comparisonWriter.println(outputLine)
        }
      }
    }
    }catch{
      case e: Exception => {
           System.err.println("Failed for comparison: "+ constraintType)
           e.printStackTrace()
         }
    }

    currentSpecTotal += constraintsRead.size
    currentSpecFound += countFound
    comparisonWriter.close()
  }

  def compareConstraintList(file: String, constraintType: String) {
    val comparisonWriter = new PrintWriter(new FileWriter(outputDir + system + "_"+ constraintType + "_accuracy.csv"))
    var countFound = 0
    var countNotFound = 0
    var constraintsRead = Set[FeatureExpr]()
    comparisonWriter.println("Code Constraint, Found")
    var count  = 1

    for (line <- Source.fromFile(file).getLines()) {

      count +=1
      val constraint = Source.fromFile(line).getLines().next().trim

      val input = new ANTLRInputStream(constraint)
      val lexer = new FExprLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new FExprParser(tokens)
      val testExpr = parser.fexpr().value

        if (testExpr == null) {
          System.err.println("null from line:" + line)
        }
        if (!constraintsRead.contains(testExpr)) {
          constraintsRead += testExpr
          var found = false

          val testFeatures = testExpr.collectDistinctFeatures

          var outputLine = line + ","

          if (testExpr.isContradiction()) {
            System.err.println(testExpr + "ITS A CONTRADITION!!")
          }

          if (testExpr.isTautology()) {
            System.err.println("LINE: " + line)
            System.err.println(testExpr + "ITS A TAUTOLOGY!!")
          }

          //testExpr should be a tautology in feature model
          if (testFeatures.subsetOf(featureModelFeatures) && (testExpr).isTautology(featureModel)) {
            found = true
            countFound += 1
            outputLine += "1"
          } else {
            outputLine += "0"
            countNotFound += 1
          }
          comparisonWriter.println(outputLine)
        }

    }

    currentSpecTotal += constraintsRead.size
    currentSpecFound += countFound
    comparisonWriter.close()
  }
}
