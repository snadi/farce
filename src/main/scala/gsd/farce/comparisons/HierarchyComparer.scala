package gsd.farce.comparisons

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import gsd.farce.utilities.PropertyKeys._
import java.util.Properties
import gsd.farce.features.model.FeatureRelationships.{EdgeType, Edge}
import gsd.farce.features.model.{FeatureRelationships, FeatureRelationshipsMain}
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
args(1): path to exconfig file
args(2): single vs combined analysis
args(3): optional prefix .. default CONFIG_
args(4): optional suffix default ""
 */
object HierarchyComparer extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  val config = getSystemConfig(args(0))

  //get boolean expressions for different parts of code
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
  val defUseExpr = featureExprParser.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  val parserExpr = featureExprParser.parseFile(properties.getProperty(PARSER_FORMULA_FILE))
  val nestedifdefExpr = featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
  val errorsExpr = featureExprParser.parseFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))
 val typeExpr = featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
  val allHardConstraints = defUseExpr.and(typeExpr).and(parserExpr).and(errorsExpr)


/*  assert(!nestedifdefExpr.and(parserExpr).isContradiction(), "Nested & parser are contra")
  assert(!nestedifdefExpr.and(typeExpr).isContradiction(), "Nested & type are contra")
  assert(!defUseExpr.and(nestedifdefExpr).isContradiction(), "Nested & def/use are contra")
  assert(!errorsExpr.and(nestedifdefExpr).isContradiction(), "Nested & errors are contra")*/
  //assert(!allHardConstraints.isContradiction(), "IT is a contradiction")
  val prefix = config.getPrefix
  val suffix = config.getSuffix

  compareEdges(args(2))

  def getExprFromEdge(e: Edge) = featureExprParser.parse(prefix+ e.a + suffix + " => " + prefix + e.b + suffix)

  def compareEdges(comparisonType: String){
    //hierarchy edges to "root" are meaningless to compare
    val relFinder = FeatureRelationships.getFeatureRelationshipFinder( args(1), prefix, suffix )
    val edges = relFinder.getEdges().filterNot( _.b == "root" )
//      FeatureRelationshipsMain.exportHierarchyEdges(args(1)).filter(x => !x.b.equals("root"))

    //get both types of edges
    val hierarchyEdges = edges.filter( _.t == EdgeType.hierarchy)
    val crossTreeEdges = edges.filter( _.t == EdgeType.crosstree)

    val hierarchyCounts = new Counts("Hierarchy")
    val crossTreeCounts = new Counts("Cross Tree")

    if (comparisonType.equals("single"))
      println("Edge, Type, Type Errors, Parser Errors,  Linker, #error, Nested ifdef")
    else if(comparisonType.equals("combined"))
      println("Edge, Type, Found")


    for (edge <- hierarchyEdges){
      hierarchyCounts.countAll+=1

      if (comparisonType.equals("single"))  {
        compareSingleExpr(edge,hierarchyCounts)
      } else if(comparisonType.equals("combined")){
        compareCombinedEdge(edge, hierarchyCounts)
      }
    }



    for (edge <- crossTreeEdges){
      crossTreeCounts.countAll+=1
      if (comparisonType.equals("single"))
        compareSingleExpr(edge, crossTreeCounts)
      else if(comparisonType.equals("combined"))
        compareCombinedEdge(edge, crossTreeCounts)
    }

    if (comparisonType.equals("combined")) {
      hierarchyCounts.printCounts(false)
      crossTreeCounts.printCounts(false)
    }    else{
      hierarchyCounts.printCounts(true)
      crossTreeCounts.printCounts(true)
    }
  }

  def compareCombinedEdge(edge: Edge, counts:Counts){
    val testExpr = getExprFromEdge(edge)

    var outputLine = testExpr + ","  + edge.t + ","

    if (allHardConstraints.implies(testExpr).isTautology()){

      outputLine += "1"
      counts.countFound +=1
    }else{
      counts.countNotFound += 1
      System.err.println("NOT FOUND: " + testExpr + ", of type: " + edge.t)
    }

    println(outputLine)
  }

  def compareSingleExpr(edge: Edge, counts:Counts) {
    val testExpr = getExprFromEdge(edge)

    var outputLine = testExpr + ","  + edge.t + ","

    var found = false

    //"Edge, Type, Found,Type Errors, Parser Errors,  Linker, #error, Nested ifdef"

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
      System.err.println("NOT FOUND: " + testExpr + ", of type: " + edge.t)
    } else {
      counts.countFound += 1
    }

    println(outputLine)
  }




  case class Counts(category: String){
    var countAll = 0
    var countFound = 0
    var countNotFound = 0
    var countErrors = 0
    var countType = 0
    var countParser= 0
    var countNested = 0
    var countDefUse = 0

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
