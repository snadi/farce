package gsd.farce.implications

import gsd.farce.features.FeatureUtils._
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExprFactory, FeatureExprParser}
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.io.{FileInputStream, File, FileWriter, PrintWriter}
import java.util.Properties
import gsd.farce.features.CreateDimacs
import gsd.linux.tools.ImplGraphMain
import gsd.farce.utilities.ImplGraphConverter
import gsd.farce.utilities.Utilities._


/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 14/03/13
 * Time: 12:35 PM
 * To change this template use File | Settings | File Templates.
 */
object ImplGraphGenerator extends App {
  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  val config = getSystemConfig(args(0))

  val prefix = config.getPrefix
  val suffix = config.getSuffix


  var featureModelImplicationGraph, typeModelGraph, defUseModelGraph, nestedIfDefGraph, errorModelGraph, parserGraph: ImplicationGraph = null
  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)


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
    ImplGraphMain.main(Array(properties.getProperty(FEATURE_MODEL_FILE),"output/FeatureModel/initialImplGraph.txt"))
    ImplGraphConverter.main(Array("output/FeatureModel/initialImplGraph.txt", properties.getProperty(FEATURE_MODEL_IMPL_GRAPH), prefix, suffix))
  }


  println("parser errors model impl graph")
  //get typesystem model implication graph
  val parserExpr = featureExprParser.parseFile(properties.getProperty(PARSER_FORMULA_FILE))
  val parserWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(PARSER_MODEL_IMPL_GRAPH))))
  parserGraph = buildImplGraph(parserExpr, "Parser Errors Model")
  parserWriter.println(parserGraph)
  parserWriter.close()

  println("def use impl graph")
  //get DEF/USE model implication graph
  val defUseExpr = featureExprParser.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  defUseModelGraph = buildImplGraph(defUseExpr, "Linker Model")
  val defUseWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(DEFUSE_MODEL_IMPL_GRAPH))))
  defUseWriter.println(defUseModelGraph)
  defUseWriter.close()

  println("Error model impl graph")
  //get typesystem model implication graph
  val errorExpr = featureExprParser.parseFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))
  val errorsModelWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(PREPROCESSOR_ERROR_MODEL_IMPL_GRAPH))))
  errorModelGraph = buildImplGraph(errorExpr, "Errors Model")
  errorsModelWriter.println(errorModelGraph)
  errorsModelWriter.close()

  println("type typesystem model impl graph")
  //get typesystem model implication graph
  val typeExpr = featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
  CreateDimacs.main(Array("--equicnf", properties.getProperty(TYPE_FORMULA_FILE), "", "", properties.getProperty(TYPE_MODEL_DIMACS)))
  println("Created dimacs")
  ImplGraphMain.main(Array(properties.getProperty(TYPE_MODEL_DIMACS),properties.getProperty(TYPE_MODEL_INITIAL_GRAPH)))
  println("Created initial")
  ImplGraphConverter.main(Array(properties.getProperty(TYPE_MODEL_INITIAL_GRAPH), properties.getProperty(TYPE_MODEL_IMPL_GRAPH)))


  println("nested ifdef model impl graph")
  //get nested ifdef implication graph
  val nestedIfDefExpr = featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
  CreateDimacs.main(Array("--equicnf", properties.getProperty(NESTED_IFDEF_FORMULA_FILE), "", "", properties.getProperty(NESTED_IFDEF_DIMACS)))
  println("Created dimacs")
  ImplGraphMain.main(Array(properties.getProperty(NESTED_IFDEF_DIMACS),properties.getProperty(NESTED_IFDEF_INITIAL_GRAPH)))
  ImplGraphConverter.main(Array(properties.getProperty(NESTED_IFDEF_INITIAL_GRAPH), properties.getProperty(NESTED_IFDEF_IMPL_GRAPH)))




  println("big conjunction impl graph")
  //get big conjunction implication graph
  //only use the hard constraints for the big conjunction
  val bigConjunction = errorExpr.and(defUseExpr).and(typeExpr).and(parserExpr).and(nestedIfDefExpr)
  val bigConjFormulaWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(BIG_CONJUNCTION_FORMULA_FILE))))
  bigConjFormulaWriter.println(bigConjunction)
  bigConjFormulaWriter.close()
  CreateDimacs.main(Array("--equicnf", properties.getProperty(BIG_CONJUNCTION_FORMULA_FILE), "", "" ,properties.getProperty(BIG_CONJUNCTION_DIMACS)))
  ImplGraphMain.main(Array(properties.getProperty(BIG_CONJUNCTION_DIMACS), properties.getProperty(BIG_CONJUNCTION_INITIAL_GRAPH)))
  ImplGraphConverter.main(Array(properties.getProperty(BIG_CONJUNCTION_INITIAL_GRAPH), properties.getProperty(BIG_CONJUNCTION_IMPL_GRAPH)))



}
