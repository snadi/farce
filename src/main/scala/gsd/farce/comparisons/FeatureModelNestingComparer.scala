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

object FeatureModelNestingComparer extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)

  //counting variables
  var countAll = 0
  var countFound = 0
  var countNotFound = 0
  var countErrors = 0
  var countType = 0
  var countParser = 0
  var countNested = 0
  var countDefUse = 0
  var countFilPc = 0
  var classify = false
  var testFilePc = true
  var testDefUse = true
  var testParser = true
  var testType = true
  var testNested = true
  var testErrors = true



  //get feature expressions for all code formulas

  //Def Use
 // var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
  //var lexer = new FExprLexer(input)
  //var tokens = new CommonTokenStream(lexer)
  //var parser = new FExprParser(tokens)
  val defUseExpr = FeatureExprFactory.True// parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  val defUseFeatures = defUseExpr.collectDistinctFeatures
//  assert(!defUseExpr.isContradiction(), "Def Use Expression is a contradiction")
  System.err.println("INFO: Read defuse")
 // if (defUseExpr.isTautology()) {
  //  System.err.println("INFO: Def Use is a tautology and will not check against it")
    testDefUse = false
  //}


  val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
  var filePcExpr:FeatureExpr = null
  var filePcFeatures = Set[String]()
  if(!lines.isEmpty){
    var input = new ANTLRInputStream(lines.next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
    filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
    filePcFeatures = filePcExpr.collectDistinctFeatures
  }


 // if (filePcExpr != null)
  //  assert(!filePcExpr.isContradiction(), "Def Use Expression is a contradiction")
  System.err.println("INFO: Read filePc")
  if (filePcExpr != null && filePcExpr.isTautology()) {
    System.err.println("INFO: No file pc or tautology and will not check against it")
    testFilePc = false
  }

  //Parser
  /*input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)*/
  var parserExpr = FeatureExprFactory.True //parser.fexpr().value//featureExprParser.parseFile(properties.getProperty(PARSER_FORMULA_FILE))
  val parserFeatures = parserExpr.collectDistinctFeatures
//  assert(!parserExpr.isContradiction(), "Parser Expression is a contradiction")
  System.err.println("INFO: read parser")
 // if (parserExpr.isTautology()) {
    testParser = false
  //  System.err.println("INFO: Parser is a tautology and will not check against it")
  //}


  //Preprocessor errors
 /* input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)*/
  val errorsExpr = FeatureExprFactory.True//parser.fexpr().value//featureExprParser.parseFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))
  val errorsFeatures = errorsExpr.collectDistinctFeatures
  //assert(!errorsExpr.isContradiction(), "Preprocessor Expression is a contradiction")
  System.err.println("INFO: read errors")
  //if (errorsExpr.isTautology()) {
    testErrors = false
  //  System.err.println("INFO: Preprocessor expression is a tautology and will not check against it")
 // }


  //type system
  //val typeModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(FEATURE_MODEL_FILE), "", "")
 /* input = new ANTLRInputStream(Source.fromFile(properties.getProperty(TYPE_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)*/
  var typeExpr = FeatureExprFactory.True//parser.fexpr().value//featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
  val typeFeatures = typeExpr.collectDistinctFeatures
 // assert(!typeExpr.isContradiction(), "Type Expression is a contradiction")
  System.err.println("INFO: read type")
 /* if (typeExpr.isTautology()) {
    testType = false
    System.err.println("INFO: Type expression is a tautology and will not check against it")
  }*/


  //Nested ifdef
  var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)).getLines().next().trim)
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  val nestedExpr =parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
  System.err.println("INFO: parsed nesting expression")
 // val nestedModel = SATFeatureModel.create(nestedExpr)
 // System.err.println("INFO: created feature model")
  // val nestedExpr = featureExprParser.parseFile("output/NestedIfdefs/nestedIfDefCombinedImplications.txt")
  //val nestedModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(NESTED_IFDEF_DIMACS), "", "")
  val nestedFeatures = nestedExpr.collectDistinctFeatures
//  assert(!nestedExpr.isContradiction(), "Nested Expression is a contradiction")
//  System.err.println("INFO: read nested")
 // if (nestedExpr.isTautology()) {
 //   testNested = false
 //   System.err.println("INFO: Nested expression is a tautology and will not check against it")
//  }

  var bigConjunction = FeatureExprFactory.True

  if (args.length > 1 && args(1).equals("combined")) {
    assert(!typeExpr.and(nestedExpr).isContradiction(), "type and nested")
    assert(!parserExpr.and(nestedExpr).isContradiction(), "parser and nested")
    assert(!errorsExpr.and(nestedExpr).isContradiction(), "errors and nested")
    assert(!defUseExpr.and(nestedExpr).isContradiction(), "defuse and nested")
    assert(!defUseExpr.and(parserExpr).isContradiction(), "defuse and parser")
    assert(!defUseExpr.and(errorsExpr).isContradiction(), "defuse and parser")
    assert(!defUseExpr.and(typeExpr).isContradiction(), "defuse and parser")


    bigConjunction = typeExpr.and(parserExpr).and(errorsExpr).and(defUseExpr).and(nestedExpr)
    System.err.println("created formula")
    assert(!bigConjunction.isTautology(), "Big conjunction is a tautology! No need for comparison")
    assert(!bigConjunction.isContradiction(), "Big conjunction is a contradiction!")

    compareCombined("hierarchy", properties.getProperty(HIERARCHY_CONSTRAINTS_FILE))
    compareCombined("cross tree", properties.getProperty(CROSSTREE_CONSTRAINTS_FILE))
    compareCombined("xor group", properties.getProperty(XOR_GROUP_FILE))
    compareCombined("or group", properties.getProperty(OR_GROUP_FILE))
    compareCombined("mutex group", properties.getProperty(MUTEX_GROUP_FILE))

    if (new File(properties.getProperty(SIMPLE_FEATURE_MODEL)).exists())
      compareCombined("simple transformation", properties.getProperty(SIMPLE_FEATURE_MODEL))
    else
      System.err.println("No simple transformation file")

    compareCombined("CNF CLauses", properties.getProperty(CNF_CLAUSES_FILE))

  } else {
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

  }

  def compareSingleExpr(line: String, relationship: String, testExpr: FeatureExpr) = {
    var outputLine = line + "," + relationship + ","
    val testFeatures = testExpr.collectDistinctFeatures

    var found = false

    //test for implication in each of the four models and alter output line accordingly
    if (testType) {
      if (testFeatures.subsetOf(typeFeatures) && typeExpr.implies(testExpr).isTautology()) {
        found = true
        countType += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testParser) {
      if (testFeatures.subsetOf(parserFeatures) && parserExpr.implies(testExpr).isTautology()) {
        found = true
        countParser += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testDefUse) {
      if (testFeatures.subsetOf(defUseFeatures) && defUseExpr.implies(testExpr).isTautology()) {
        found = true
        countDefUse += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }


    if (testErrors) {
      if (testFeatures.subsetOf(errorsFeatures) && errorsExpr.implies(testExpr).isTautology()) {
        found = true
        countErrors += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }


    if (testNested) {
      if (testFeatures.subsetOf(nestedFeatures) && nestedExpr.implies(testExpr).isTautology()) {
        found = true
        countNested += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

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
    countErrors = 0
    countType = 0
    countParser = 0
    countNested = 0
    countDefUse = 0
  }

  def compareIndividual(relation: String, fileToCompare: String) = {
    resetCounts()
    System.err.println("==================================================")
    if (!new java.io.File(fileToCompare).exists) {
      System.err.println(fileToCompare + " does not exist")
    } else {

      println("Constraint, RelationshipType, Type, Parser, Linker, #error, Nested ifdef, file pc")
      val featureExprParser = new FeatureExprParser()
      for (line <- Source.fromFile(fileToCompare).getLines()) {

        val testExpr = featureExprParser.parse(line)
        countAll += 1
      //  if (!testExpr.isTautology() && !testExpr.isContradiction()) {
          compareSingleExpr(line, relation, testExpr)
      //  }
      }

      System.err.println("total " + relation + " constraints in feature model: " + countAll)
      System.err.println("total " + relation + " constraints in feature model found in code: " + countFound)
      System.err.println("total " + relation + " constraints in feature model not found in code: " + countNotFound)
      System.err.println("total " + relation + " constraints in feature model found in type errors:  " + countType)
      System.err.println("total " + relation + "constraints in feature model found in parser errors:  " + countParser)
      System.err.println("total " + relation + " constraints in feature model found in nested ifdefs: " + countNested)
      System.err.println("total " + relation + " constraints in feature model found in errors: " + countErrors)
      System.err.println("total " + relation + " constraints in feature mdoel found in def/use: " + countDefUse)
      System.err.println("total " + relation + " constraints in feature mdoel found file pc: " + countFilPc)

    }
    System.err.println("==================================================")
  }

  def compareCombinedExpr(line: String, relationship: String, testExpr: FeatureExpr) = {
    var outputLine = line + "," + relationship + ","
    val implication = bigConjunction.implies(testExpr)
    //test for implication in conjunction of the four models and alter output line accordingly
    if (implication.isTautology()) {
      countFound += 1
      outputLine += "1"
    } else {
      outputLine += "0"
      System.err.println("NOT FOUND: " + line)
      countNotFound += 1
    }

    println(outputLine)
  }

  def compareCombined(relation: String, fileToCompare: String) = {
    resetCounts()


    println("Constraint, Relationship, Found")
    val featureExprParser = new FeatureExprParser()
    for (line <- Source.fromFile(fileToCompare).getLines()) {

      val testExpr = featureExprParser.parse(line)
      countAll += 1
      compareCombinedExpr(line, relation, testExpr)
    }


    System.err.println("total " + relation + " constraints in feature model: " + countAll)
    System.err.println("total " + relation + " constraints in feature model found in code: " + countFound)
    System.err.println("total " + relation + " constraints in feature model not found in code: " + countNotFound)
  }

}

