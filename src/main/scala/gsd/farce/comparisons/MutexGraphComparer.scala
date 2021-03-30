package gsd.farce.comparisons

import de.fosd.typechef.featureexpr._
import sat.{SATFeatureExpr, SATFeatureModel}
import gsd.farce.utilities.PropertyKeys._
import java.io._
import java.util.Properties
import gsd.farce.implications.{MutexGraph, ImplicationGraph}
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/02/13
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */
class MutexGraphComparer(featureModelMutexGraph: MutexGraph) {

  /*
  Compares the feature model implication graph to a list of other implication graphs
   generated from different models. So far these are: the parsing/type typesystem model, linker information, and nested ifdefs .
   Output is in comma seperated format
   */
  def compareImplications(graphsToCompare: Array[MutexGraph]) = {

    print("Mutex Edge, ")
    graphsToCompare.map(x => print(x.getName + ","))
    print("\n")
    var totalFound = 0
    val countsFound = new Array[Int](graphsToCompare.length)


    for (edge <- featureModelMutexGraph.getEdges) {
      edge.initializeCompArray(graphsToCompare.length)
      var index = 0
      var found = false
      for (graph <- graphsToCompare) {
        if (graph.containsEdge(edge)) {
          countsFound(index) += 1
          edge.setComparison(index)
          found = true
        }
        index += 1
      }

      if(found)
        totalFound +=1
      edge.printComparison()
    }

    System.err.println("Num. of implications in feature model: " + featureModelMutexGraph.getImplGraphSize)
    graphsToCompare.map(x => System.err.println("Num. of mutual exclusions in " + x.getName + ": " + x.getImplGraphSize))
    (0 to (graphsToCompare.length - 1)) map (x => System.err.println("Num. of feature model mutual exclusions found in " + graphsToCompare(x).getName + " mutex graph: " + countsFound(x)))
    System.err.println("TOTAL FOUND: " + totalFound)
  }
}

object MutexGraphComparer {

  def createGraphFromFile(graphName: String, fileName: String): MutexGraph = {
    val mutexGraph = new MutexGraph(graphName)
    mutexGraph.createGraphFromFile(fileName)

    mutexGraph

  }

  def main(args: Array[String]) {

    val properties = new Properties()

    loadPropertiesFile(args(0), properties)

    val featureModelMutexGraph = createGraphFromFile("Feature Model", properties.getProperty(FEATURE_MODEL_MUTEX_GRAPH))
    val modelComparer = new MutexGraphComparer(featureModelMutexGraph)

    System.err.println("Num. of mutual excl. in feature model mutex graph: " + featureModelMutexGraph.getImplGraphSize)

    def compareIndividual() = {

      println("in individual")
      val typeModelMutexGraph = createGraphFromFile("Type Errors Model", properties.getProperty(TYPE_MODEL_MUTEX_GRAPH))
      val parserErrorModelGraph = createGraphFromFile("Parsing Errors Model", properties.getProperty(PARSER_MODEL_MUTEX_GRAPH))
      val defUseModelMutexGraph = createGraphFromFile("Def/Use Model", properties.getProperty(DEFUSE_MODEL_MUTEX_GRAPH))
      val errorModelGraph = createGraphFromFile("Errors Model", properties.getProperty(PREPROCESSOR_ERROR_MODEL_MUTEX_GRAPH))
      val nestedIfDefMutexGraph = createGraphFromFile("NestedIfDefModel Model", properties.getProperty(NESTED_IFDEF_MUTEX_GRAPH))
      //compare implications in feature model to those in all other models

      modelComparer.compareImplications(Array(typeModelMutexGraph, defUseModelMutexGraph, errorModelGraph, nestedIfDefMutexGraph, parserErrorModelGraph))
    }

    def compareCombined() = {
      val bigConjGraph = createGraphFromFile("All Code Model", properties.getProperty(BIG_CONJUNCTION_MUTEX_GRAPH))
      modelComparer.compareImplications(Array(bigConjGraph))
    }

    if (args.length > 1 && args(1).equals("combined")) {
      compareCombined()
    } else {
      compareIndividual()
    }
  }
}

