package gsd.farce.comparisons

import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.PropertyKeys._
import java.io._
import java.util.Properties
import io.Source
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/02/13
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */

/*
  Checks if the feature model implications directly extracted from Kconfig (featureModel file) are satisfiable in the
    formulas extracted from the code from the four sources of information: parsing/type typesystem model,
    linker information, nested ifdefs, and errors.

  Args: 1) system to compare 2) "individual" to compare individual sources, "combined" to compare big conjunction. Default: individual.
   Output is in comma seperated format withCNF  1 if the model contains the implication and 0 otherwise, and stats go to System.err
   */

object FeatureModelComparerFilePc extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)

  //counting variables
  var countAll = 0
  var countFound = 0
  var countNotFound = 0
  var countFilPc = 0
  var classify = false
  var testFilePc = true



  var input = new ANTLRInputStream(Source.fromFile("output/FilePcs/filePcFormula.txt").getLines().next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  val filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  val filePcFeatures = filePcExpr.collectDistinctFeatures
 assert(!filePcExpr.isContradiction(), "Def Use Expression is a contradiction")
  System.err.println("INFO: Read filePc")
  if (filePcExpr.isTautology()) {
    System.err.println("INFO: Def Use is a tautology and will not check against it")
    testFilePc = false
  }


    compareIndividual("hierarchy", properties.getProperty(HIERARCHY_CONSTRAINTS_FILE))
    compareIndividual("cross tree", properties.getProperty(CROSSTREE_CONSTRAINTS_FILE))
    compareIndividual("xor group", properties.getProperty(XOR_GROUP_FILE))
    compareIndividual("or group", properties.getProperty(OR_GROUP_FILE))
    compareIndividual("mutex group", properties.getProperty(MUTEX_GROUP_FILE))

    if (properties.getProperty(SIMPLE_FEATURE_MODEL) != null && !properties.getProperty(SIMPLE_FEATURE_MODEL).isEmpty)
      compareIndividual("simple transformation", properties.getProperty(SIMPLE_FEATURE_MODEL))
    else
      System.err.println("No simple transformation file")

    //compare CNF clauses translated into a .constraints file
    compareIndividual("CNF CLauses", properties.getProperty(CNF_CLAUSES_FILE))



  def compareSingleExpr(line: String, relationship: String, testExpr: FeatureExpr) = {
    var outputLine = line + "," + relationship + ","
    val testFeatures = testExpr.collectDistinctFeatures

    var found = false

    //test for implication in each of the four models and alter output line accordingly
    if (testFilePc) {
      if (testFeatures.subsetOf(filePcFeatures) && filePcExpr.implies(testExpr).isTautology()) {
        found = true
        countFilPc += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    //update counts
    if (!found) {
      countNotFound += 1
      System.err.println("NOT FOUND: " + line)
    } else {
      countFound += 1
    }

    println(outputLine)
  }

  def resetCounts() {
    //counting variables
    countAll = 0
    countFound = 0
    countNotFound = 0
    countFilPc = 0
  }

  def compareIndividual(relation: String, fileToCompare: String) = {
    resetCounts()
    System.err.println("==================================================")
    if (!new java.io.File(fileToCompare).exists) {
      System.err.println(fileToCompare + " does not exist")
    } else {

      println("Constraint, RelationshipType, Type, Parser, Linker, #error, Nested ifdef")
      val featureExprParser = new FeatureExprParser()
      for (line <- Source.fromFile(fileToCompare).getLines()) {

        val testExpr = featureExprParser.parse(line)
        countAll += 1
        if (!testExpr.isTautology() && !testExpr.isContradiction()) {
          compareSingleExpr(line, relation, testExpr)
        }
      }

      System.err.println("total " + relation + " constraints in feature model: " + countAll)
      System.err.println("total " + relation + " constraints in feature model found in code: " + countFound)
      System.err.println("total " + relation + " constraints in feature model not found in code: " + countNotFound)
      System.err.println("total " + relation + " constraints in feature model found in file pc:  " + countFilPc)

    }
    System.err.println("==================================================")
  }

}

