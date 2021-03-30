package gsd.farce.comparisons

import io.Source
import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.util.Properties
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import gsd.farce.utilities.Utilities._
import gsd.farce.features.DimacsReader

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 14/03/13
 * Time: 10:14 AM
 * To change this template use File | Settings | File Templates.
 */
object DimacsConstraintComparer extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  val config = getSystemConfig(args(0))

  var countFound = 0
  var countNotFound = 0


  var constraintsRead = Set[FeatureExpr]()
  var featureModel: SATFeatureModel = null


  if (properties.getProperty(FEATURE_MODEL_FILE).contains(".dimacs")) {
    featureModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(FEATURE_MODEL_FILE), config.getPrefix, config.getSuffix)
  } else {
    val fexpr = new FeatureExprParser(FeatureExprFactory.sat).parseFile(properties.getProperty(FEATURE_MODEL_FILE)).asInstanceOf[SATFeatureExpr]
    featureModel = SATFeatureModel.create(fexpr).asInstanceOf[SATFeatureModel]
  }

  var fileList = Source.fromFile(args(1)).getLines().toList
  println("filelist size:" + fileList.size)


  val featureModelFeatures = featureModel.variables.keys.toSet
  println("Code Constraint, Found")


  for (file <- fileList) {
    val dimacsReader = new DimacsReader()
    dimacsReader.readDimacs(file)
    val testExpr = dimacsReader.getFeatureExpr

    if (!constraintsRead.contains(testExpr)) {
      constraintsRead += testExpr
      var found = false

      val testFeatures = testExpr.collectDistinctFeatures.filter(!_.startsWith("__fresh"))

      //TODO: comparison will not work correctly with formulas with generated features
      //testExpr should be a tautology in feature model
      if (testFeatures.subsetOf(featureModelFeatures) && (testExpr).isTautology(featureModel)) {
        found = true
        println(file + "," + 1)
        countFound += 1
      } else {
        countNotFound += 1
        println(file + "," + 0)
        System.err.println("NOT FOUND: " + file)
      }
    }
  }

  System.err.println("Num. of unique implications extracted: " + constraintsRead.size)
  System.err.println("Num. of unique implications found in Feature Model formula: " + countFound)
  System.err.println("Num. of unique implications not found in Feature Model formula: " + countNotFound)
}
