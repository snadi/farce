package gsd.farce.comparisons

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import gsd.farce.utilities.PropertyKeys._
import java.util.Properties
import gsd.farce.features.model.FeatureRelationships.Edge
import gsd.farce.features.model.{FeatureGroupsMain, FeatureRelationshipsMain}
import gsd.farce.features.model.FeatureGroupsMain.{FeatureGroup, GroupType}
import gsd.farce.comparisons.HierarchyComparer.Counts
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/04/13
 * Time: 2:46 PM
 * To change this template use File | Settings | File Templates.
 */
/*
usage: args(0) : name of system
args(1): path to exconfig or iml file
args(2): single vs combined analysis
args(3): optional prefix .. default CONFIG_
args(4): opetions suffix default ""
 */
object GroupComparer extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  val config = getSystemConfig(args(0))

  //get boolean expressions for different parts of code
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
  val defUseExpr = featureExprParser.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  val typeExpr = featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
  val parserExpr = featureExprParser.parseFile(properties.getProperty(PARSER_FORMULA_FILE))
  val nestedifdefExpr = featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
  val errorsExpr = featureExprParser.parseFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))
  val allHardConstraints = defUseExpr.and(typeExpr).and(errorsExpr).and(parserExpr)
  val comparisonType = args(2)

  val prefix = config.getPrefix
  val suffix = config.getSuffix

  compareGroups()

  def getExprFromEdge(e: Edge) = featureExprParser.parse(prefix + e.a + suffix + " => " + prefix + e.b + suffix)

  def compareGroups() {

    val groups = if (args(1).endsWith(".iml")) {
      FeatureGroupsMain.getCDLGroups(args(1), prefix, suffix)
    } else {
      FeatureGroupsMain.getKconfigGroups(args(1), prefix, suffix)
    }


    val orGroup = groups.filter(x => x.gtype == GroupType.gOR)

    val xorGroup = groups.filter(x => x.gtype == GroupType.gXOR)

    val mutexGroup = groups.filter(x => x.gtype == GroupType.gMUTEX)

    val mutexCounts = new Counts("mutex")
    val orCounts = new Counts("or")
    val xorCounts = new Counts("xor")

    compareGroup(xorGroup, xorCounts)
    compareGroup(orGroup, orCounts)
    compareGroup(mutexGroup, mutexCounts)
    System.err.println("xor: " + xorGroup.size + " or: " + orGroup.size + " mutex: " + mutexGroup.size)

    if (comparisonType.equals("combined")) {
      mutexCounts.printCounts(false)
      orCounts.printCounts(false)
      xorCounts.printCounts(false)
    } else {
      mutexCounts.printCounts(true)
      orCounts.printCounts(true)
      xorCounts.printCounts(true)
    }
  }

  def compareGroup(groups: List[FeatureGroup], counts: Counts) {

    if (comparisonType.equals("single")) {
      println(counts.category + " Group Constraint, Type, Parser, Linker, #error, nesting")
    }
    else if (comparisonType.equals("combined")) {
      println(counts.category + " Group constraint, Found")
    }

    for (group <- groups) {

      var testExpr = FeatureExprFactory.True

      counts.countAll += 1

      if (counts.category.equals("xor")) {
        testExpr = featureExprParser.oneOf(group.groupedFeatures.map(featureExprParser.parse(_)))
      } else if (counts.category.equals("or")) {
        testExpr = featureExprParser.atLeastOne(group.groupedFeatures.map(featureExprParser.parse(_)))
      } else if (counts.category.equals("mutex")) {
        testExpr = featureExprParser.atMostOne(group.groupedFeatures.map(featureExprParser.parse(_)))
      } else {
        System.err.println("NO TEST EXPR. INVALID GROUP TYPE")
      }

      if (comparisonType.equals("single")) {
        compareSingleExpr(testExpr, counts)
      }
      else if (comparisonType.equals("combined")) {
        compareCombinedEdge(testExpr, counts)
      }
    }
  }

  def compareCombinedEdge(testExpr: FeatureExpr, counts: Counts) {

    if (allHardConstraints.implies(testExpr).isTautology()) {
      counts.countFound += 1
    } else {
      counts.countNotFound += 1
      System.err.println("NOT FOUND: " + testExpr)
    }
  }

  def compareSingleExpr(testExpr: FeatureExpr, counts: Counts) {


    var outputLine = testExpr + "," + counts.category + ","

    var found = false

    //test for implication in each of the four models and alter output line accordingly
    if (typeExpr.implies(testExpr).isTautology()) {
      found = true
      counts.countType += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    if (parserExpr.implies(testExpr).isTautology()) {
      found = true
      counts.countParser += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    if (defUseExpr.implies(testExpr).isTautology()) {
      found = true
      counts.countDefUse += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }


    if (errorsExpr.implies(testExpr).isTautology()) {
      found = true
      counts.countErrors += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }


    if (nestedifdefExpr.implies(testExpr).isTautology()) {
      found = true
      counts.countNested += 1
      outputLine += "1,"
    } else {
      outputLine += "0,"
    }

    //update counts
    if (!found) {
      counts.countNotFound += 1
      System.err.println("NOT FOUND: " + testExpr)
    } else {
      counts.countFound += 1
    }

    println(outputLine)
  }


  case class Counts(category: String) {
    var countAll = 0
    var countFound = 0
    var countNotFound = 0
    var countErrors = 0
    var countType = 0
    var countNested = 0
    var countDefUse = 0
    var countParser = 0

    def printCounts(printIndivid: Boolean) {
      System.err.println("total " + category + " constraints in feature model: " + countAll)
      System.err.println("total " + category + " constraints in feature model found in code: " + countFound)
      System.err.println("total " + category + " constraints in feature model not found in code: " + countNotFound)

      if (printIndivid) {
        System.err.println("total " + category + " constraints in feature model found in type errors:  " + countType)
        System.err.println("total " + category + " constraints in feature model found in parser errors:  " + countParser)
        System.err.println("total " + category + " constraints in feature model found in nested ifdefs: " + countNested)
        System.err.println("total " + category + " constraints in feature model found in errors: " + countErrors)
        System.err.println("total " + category + " constraints in feature mdoel found in def/use: " + countDefUse)
      }
    }
  }

}
