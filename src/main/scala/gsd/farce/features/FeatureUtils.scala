package gsd.farce.features

import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import gsd.farce.implications.{Implication, ImplicationGraph}
import org.sat4j.core.{VecInt, Vec}
import org.sat4j.specs.IVecInt

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 13/02/13
 * Time: 3:14 PM
 * To change this template use File | Settings | File Templates.
 */
object FeatureUtils {

  def isImplication(featureExpr: FeatureExpr, var1: String, var2: String): Boolean = {
    val feature1 = FeatureExprFactory.createDefinedExternal(var1)
    val feature2 = FeatureExprFactory.createDefinedExternal(var2)


    return ((feature1.and(!feature2)).and(featureExpr)).isContradiction()

  }

  def isMutualExclusion(featureExpr: FeatureExpr, var1: String, var2: String): Boolean = {
    val feature1 = FeatureExprFactory.createDefinedExternal(var1)
    val feature2 = FeatureExprFactory.createDefinedExternal(var2)


    return ((feature1.and(feature2)).and(featureExpr)).isContradiction()

  }


  def isImplicationInFM(featureModel: FeatureModel, var1: String, var2: String): Boolean = {
    val feature1 = FeatureExprFactory.createDefinedExternal(var1)
    val feature2 = FeatureExprFactory.createDefinedExternal(var2)

    return (feature1.and(!feature2)).isContradiction(featureModel)
  }

  def isMutualExclusion(featureModel: FeatureModel, var1: String, var2: String): Boolean = {
    val feature1 = FeatureExprFactory.createDefinedExternal(var1)
    val feature2 = FeatureExprFactory.createDefinedExternal(var2)

    return (feature1.and(feature2)).isContradiction(featureModel)
  }

  def isFeatureDead(featureName: String, featureExpr: FeatureExpr): Boolean = (FeatureExprFactory.createDefinedExternal(featureName).and(featureExpr)).isContradiction()
  def isFeatureDead(featureName: String, featureModel: FeatureModel): Boolean = (FeatureExprFactory.createDefinedExternal(featureName)).isContradiction(featureModel)

  def isFeatureDeadInFM(featureName: String, featureModel: FeatureModel): Boolean = FeatureExprFactory.createDefinedExternal(featureName).isContradiction(featureModel)

  def buildImplGraph(featureModel: SATFeatureModel, graphName: String): ImplicationGraph = {
    val implGraph = new ImplicationGraph(graphName)

    val distinctFeatures = removeDeadAndGeneratedFeatures(featureModel.variables.keys, featureModel)
                      println("distinct features: " + distinctFeatures.size)
    var count1 = 0
    var count2 = 0

    for (variable1 <- distinctFeatures) {
      count1+=1
      println("count: " + count1)
      for (variable2 <- distinctFeatures) {
        count2 += 1
        if (!variable1.equals(variable2) && isImplicationInFM(featureModel, variable1, variable2)) {
          val implication = new Implication(variable1, variable2)
          implGraph.addImplication(implication)
        }
      }
    }

    implGraph
  }

  def buildExclGraph(featureModel: SATFeatureModel, graphName: String): ImplicationGraph = {
    val implGraph = new ImplicationGraph(graphName)

    val distinctFeatures = removeDeadAndGeneratedFeatures(featureModel.variables.keys, featureModel)

    for (variable1 <- distinctFeatures) {
      for (variable2 <- distinctFeatures) {
        if (!variable1.equals(variable2) && isMutualExclusion(featureModel, variable1, variable2)) {
          implGraph.addImplication(new Implication(variable1, "!" + variable2))
        }
      }
    }

    implGraph
  }

  /*
  removes dead features from a list of features according to input feature expression
  returns new set with dead features removed
   */
  def removeDeadAndGeneratedFeatures(features: Set[String], featureExpr: FeatureExpr): Set[String] = {
    var featureSet:Set[String] = Set[String]()

    for (feature <- features){
      if(!feature.contains("$__fresh") && !isFeatureDead(feature, featureExpr)){
          featureSet += feature
      }
    }

    featureSet
  }

  def removeDeadAndGeneratedFeatures(features: Iterable[String], featureModel: FeatureModel): Set[String] = {
    var featureSet:Set[String] = Set[String]()

    for (feature <- features){
      if(!feature.contains("$__fresh") && !isFeatureDead(feature, featureModel)){
        if(feature.startsWith("CONFIG_"))
          featureSet += feature
        else
          featureSet += "CONFIG_" + feature
      }
    }

    featureSet
  }

  def buildImplGraph(featureExpr: FeatureExpr, graphName: String): ImplicationGraph = {
                          println("starting build")
    val implGraph = new ImplicationGraph(graphName)

    val distinctFeatures = removeDeadAndGeneratedFeatures(featureExpr.collectDistinctFeatures, featureExpr)

    for (variable1 <- distinctFeatures) {
        for (variable2 <- distinctFeatures) {
          if (!variable1.equals(variable2) && isImplication(featureExpr, variable1, variable2)) {
            implGraph.addImplication(new Implication(variable1, variable2))
          }
        }
      }

    implGraph
  }


  def buildExclGraph(featureExpr: FeatureExpr, graphName: String): ImplicationGraph = {
    println("starting build")
    val implGraph = new ImplicationGraph(graphName)

    val distinctFeatures = removeDeadAndGeneratedFeatures(featureExpr.collectDistinctFeatures, featureExpr)

    for (variable1 <- distinctFeatures) {
      for (variable2 <- distinctFeatures) {
        if (!variable1.equals(variable2) && isMutualExclusion(featureExpr, variable1, variable2)) {
          implGraph.addImplication(new Implication(variable1, "!" + variable2))
        }
      }
    }

    implGraph
  }

  def getVariableMap(file: String, variablePrefix: String = "", suffix: String = "") = {
    var variables: Map[String, Int] = Map()
    val clauses = new Vec[IVecInt]()
    var maxId = 0

    for (line <- scala.io.Source.fromFile(file).getLines) {
      if (line startsWith "c ") {
        val entries = line.substring(2).split(" ")
        val id = if (entries(0) endsWith "$")
          entries(0).substring(0, entries(0).length - 1).toInt
        else if (entries(0) startsWith "$")
          entries(0).substring(1).toInt
        else
          entries(0).toInt
        maxId = scala.math.max(id, maxId)
        variables = variables.updated(variablePrefix + entries(1) + suffix, id)

      } else if ((line startsWith "p ") || (line.trim.size == 0)) {
        //comment, do nothing
      } else {
        val vec = new VecInt()
        val parts = line.split(" ")
        var index = 0 //use the parts length to avoid ending in 0 vs ending in 1 problem
        while (index < parts.length - 1) {
          //if (literal != "1")
          vec.push(parts(index).toInt)
          index += 1
        }
        assert(!vec.isEmpty)
        clauses.push(vec)
      }

    }
    assert(maxId == variables.size, "largest variable id " + maxId + " differs from number of variables " + variables.size)

    variables
  }

}

