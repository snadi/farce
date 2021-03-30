package gsd.farce.comparisons

import de.fosd.typechef.featureexpr._
import sat.{SATFeatureExpr, SATFeatureModel}
import gsd.farce.utilities.PropertyKeys._
import java.io._
import java.util.Properties
import gsd.farce.implications.ImplicationGraph
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/02/13
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */
class ImplicationGraphComparer(featureModelImplGraph: ImplicationGraph) {

  /*
  Compares the feature model implication graph to a list of other implication graphs
   generated from different models. So far these are: the parsing/type typesystem model, linker information, and nested ifdefs .
   Output is in comma seperated format
   */
  def compareImplications(graphsToCompare: Array[ImplicationGraph]) = {

    print("Implication, ")
    graphsToCompare.map(x => print(x.getName + ","))
    print("\n")

    var totalFound = 0
    val countsFound = new Array[Int](graphsToCompare.length)


    for (implication <- featureModelImplGraph.getImplications) {
      implication.initializeCompArray(graphsToCompare.length)
      var index = 0
      var found = false
      for (graph <- graphsToCompare) {
        if (graph.containsImplication(implication)) {
          countsFound(index) += 1
          implication.setComparison(index)
          found = true
        }
        index += 1
      }

      if(found)
        totalFound +=1

      implication.printComparison()
    }

    System.err.println("Num. of implications in feature model: " + featureModelImplGraph.getImplGraphSize)
    graphsToCompare.map(x => System.err.println("Num. of implications in " + x.getName + ": " + x.getImplGraphSize))
    (0 to (graphsToCompare.length - 1)) map (x => System.err.println("Num. of feature model implications found in " + graphsToCompare(x).getName + " impl. graph: " + countsFound(x)))
    System.err.println("TOTAL FOUND: " + totalFound)
  }
}

object ImplicationGraphComparer {

  def createGraphFromFile(graphName: String, fileName: String): ImplicationGraph = {
    val implicationGraph = new ImplicationGraph(graphName)
    implicationGraph.createGraphFromFile(fileName)

    implicationGraph

  }

  def main(args: Array[String]) {

    val properties = new Properties()
    loadPropertiesFile(args(0), properties)

    val featureModelImplicationGraph = createGraphFromFile("Feature Model", properties.getProperty(FEATURE_MODEL_IMPL_GRAPH))
    val modelComparer = new ImplicationGraphComparer(featureModelImplicationGraph)

    System.err.println("Num. of implications in feature model impl. graph: " + featureModelImplicationGraph.getImplGraphSize)

    def compareIndividual() = {

      println("in individual")
      val typeErrorModelGraph = createGraphFromFile("Type Errors Model", properties.getProperty(TYPE_MODEL_IMPL_GRAPH))
      val parserErrorModelGraph = createGraphFromFile("Parsing Errors Model", properties.getProperty(PARSER_MODEL_IMPL_GRAPH))
      val defUseModelGraph = createGraphFromFile("Def/Use Model", properties.getProperty(DEFUSE_MODEL_IMPL_GRAPH))
      val errorModelGraph = createGraphFromFile("Errors Model", properties.getProperty(PREPROCESSOR_ERROR_MODEL_IMPL_GRAPH))
      val nestedIfDefGraph = createGraphFromFile("NestedIfDefModel Errors Model", properties.getProperty(NESTED_IFDEF_IMPL_GRAPH))

      println("loaded all graphs")
      //compare implications in feature model to those in all other models

      modelComparer.compareImplications(Array(typeErrorModelGraph, defUseModelGraph, errorModelGraph, nestedIfDefGraph, parserErrorModelGraph))
    }

    def compareCombined() = {
      val bigConjGraph = createGraphFromFile("All Code Model", properties.getProperty(BIG_CONJUNCTION_IMPL_GRAPH))
      modelComparer.compareImplications(Array(bigConjGraph))
    }

    if (args.length > 1 && args(1).equals("combined")) {
      compareCombined()
    } else {
      compareIndividual()
    }
  }
}

