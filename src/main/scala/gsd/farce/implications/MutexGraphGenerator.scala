package gsd.farce.implications

import gsd.farce.features.FeatureUtils._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.io.{FileInputStream, File, FileWriter, PrintWriter}
import java.util.Properties
import gsd.farce.features.CreateDimacs
import gsd.linux.tools.{MutexGraphMain, ImplGraphMain}
import gsd.farce.utilities.ImplGraphConverter
import gsd.farce.utilities.PropertyKeys._
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 14/03/13
 * Time: 12:35 PM
 * To change this template use File | Settings | File Templates.
 */
object MutexGraphGenerator extends App {
  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  val config = getSystemConfig(args(0))

  val prefix = config.getPrefix
  val suffix = config.getSuffix

  var featureModelImplicationGraph, typeModelGraph, defUseModelGraph, nestedIfDefGraph: ImplicationGraph = null


  //get feature model mutex graph

  var featureModel:SATFeatureModel = null
  println("feature model impl graph")
  //get feature model implication graph
  if(!properties.getProperty(FEATURE_MODEL_FILE).endsWith("dimacs")){
    val fexpr = new FeatureExprParser(FeatureExprFactory.sat).parseFile(properties.getProperty(FEATURE_MODEL_FILE)).asInstanceOf[SATFeatureExpr]
    featureModel = SATFeatureModel.create(fexpr).asInstanceOf[SATFeatureModel]
    featureModelImplicationGraph = buildImplGraph(featureModel, "Feature Model")
    val featureModelWriter = new PrintWriter(new FileWriter(properties.getProperty(FEATURE_MODEL_IMPL_GRAPH)))
    featureModelWriter.println(featureModelImplicationGraph)
    featureModelWriter.close()
  }else{
    println("created from dimacs")
    MutexGraphMain.main(Array(properties.getProperty(FEATURE_MODEL_FILE),"output/FeatureModel/initialMutexGraph.txt"))
    ImplGraphConverter.main(Array("output/FeatureModel/initialMutexGraph.txt", properties.getProperty(FEATURE_MODEL_MUTEX_GRAPH), prefix, suffix))
  }

  //get typesystem model mutex graph
  println("type system model")
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
 val typeExpr = featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
    CreateDimacs.main(Array("--cnf", properties.getProperty(TYPE_FORMULA_FILE), "", "", "output/TypeErrors/typeErrorModel.dimacs"))
println("CREATED TYPE DIMACS")
MutexGraphMain.main(Array("output/TypeErrors/typeErrorModel.dimacs", "output/TypeErrors/typeErrorsInitialMutex.txt"))
ImplGraphConverter.main(Array("output/TypeErrors/typeErrorsInitialMutex.txt", properties.getProperty(TYPE_MODEL_MUTEX_GRAPH)))


  //get parser errors mutex graph
  println("parser model")
  val parserExpr = featureExprParser.parseFile(properties.getProperty(PARSER_FORMULA_FILE))
  CreateDimacs.main(Array("--cnf", properties.getProperty(TYPE_FORMULA_FILE), "", "", "output/ParserErrors/parserErrorModel.dimacs"))
  println("CREATED TYPE DIMACS")
  MutexGraphMain.main(Array("output/ParserErrors/parserErrorModel.dimacs", "output/ParserErrors/parserErrorsInitialMutex.txt"))
  ImplGraphConverter.main(Array("output/ParserErrors/parserErrorsInitialMutex.txt", properties.getProperty(PARSER_MODEL_MUTEX_GRAPH)))



  println("nested ifdef")
//get nested ifdef mutex graph
val nestedIfDefExpr = featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
MutexGraphMain.main(Array(properties.getProperty(NESTED_IFDEF_DIMACS), properties.getProperty(NESTED_IFDEF_INITIAL_MUTEX_GRAPH)))
ImplGraphConverter.main(Array(properties.getProperty(NESTED_IFDEF_INITIAL_MUTEX_GRAPH), properties.getProperty(NESTED_IFDEF_MUTEX_GRAPH)))

println("def use")
//get DEF/USE model mutex graph
val defUseExpr = featureExprParser.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
 CreateDimacs.main(Array("--cnf", properties.getProperty(DEFUSE_FORMULA_FILE), "", "", "output/DefUse/defuse.dimacs"))
 MutexGraphMain.main(Array( "output/DefUse/defuse.dimacs", "output/DefUse/defUseInitialMutex.txt"))
 ImplGraphConverter.main(Array("output/DefUse/defUseInitialMutex.txt", properties.getProperty(DEFUSE_MODEL_MUTEX_GRAPH)))

  println("errors")
  //get DEF/USE model mutex graph
  val errorExpr = featureExprParser.parseFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))
  CreateDimacs.main(Array("--cnf", properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE), "", "", "output/Errors/errors.dimacs"))
  MutexGraphMain.main(Array("output/Errors/errors.dimacs", "output/Errors/errorsInitialMutex.dimacs"))
  ImplGraphConverter.main(Array("output/Errors/errorsInitialMutex.dimacs", properties.getProperty(PREPROCESSOR_ERROR_MODEL_MUTEX_GRAPH)))

  println("big conjunction")
  //get big conjunction mutex graph
  //only use hard constraints in the big conjunction
  val bigConjunction = typeExpr.and(defUseExpr).and(errorExpr).and(parserExpr)
  MutexGraphMain.main(Array(properties.getProperty(BIG_CONJUNCTION_DIMACS), properties.getProperty(BIG_CONJUNCTION_INITIAL_MUTEX_GRAPH)))
  ImplGraphConverter.main(Array(properties.getProperty(BIG_CONJUNCTION_INITIAL_MUTEX_GRAPH), properties.getProperty(BIG_CONJUNCTION_MUTEX_GRAPH)))
}
