package gsd.farce.comparisons

import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import sat.SATFeatureModel
import gsd.farce.utilities.PropertyKeys._
import java.util.Properties
import io.Source
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import org.sat4j.specs.ISolver
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

object FilterModelEdgesLinux extends App {

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)

  val constraintsFile = args(1)

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
/*  var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  val defUseExpr = parser.fexpr().value
  val defUseFeatures = defUseExpr.collectDistinctFeatures*/

  //Parser
  var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  var parserExpr = parser.fexpr().value
  val parserFeatures = parserExpr.collectDistinctFeatures
  System.err.println("INFO: read parser")

  val defUseModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(DEFUSE_DIMACS_FILE), "", "")
  val defUseFeatures = defUseModel.variables.keys.toSet.filter(x => !x.startsWith("__fresh"))
  System.err.println("INFO: Read defuse")

  val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
  var filePcExpr: FeatureExpr = null
  var filePcFeatures = Set[String]()
  if (!lines.isEmpty) {
    input = new ANTLRInputStream(lines.next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
    filePcFeatures = filePcExpr.collectDistinctFeatures
  }
  System.err.println("INFO: Read filePc")
  if (filePcExpr == null) {
    System.err.println("INFO: No file pc ")
    testFilePc = false
  } else if (filePcExpr.isTautology()) {
    System.err.println("INFO: File pc is tautology, will not check against it ")
    testFilePc = false
  }



  //Preprocessor errors
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  val errorsExpr = parser.fexpr().value
  val errorsFeatures = errorsExpr.collectDistinctFeatures
  System.err.println("INFO: read errors")


  //type system
  val typeModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(TYPE_MODEL_DIMACS), "", "")
  val typeFeatures = typeModel.variables.keys.toSet.filter(x => !x.startsWith("__fresh"))
  System.err.println("INFO: read type")


  //Nested ifdef
  val nestingModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(NESTED_IFDEF_DIMACS), "", "")
  val nestedFeatures = nestingModel.variables.keys.toSet.filter(x => !x.startsWith("__fresh"))
  System.err.println("INFO: read nesting")

  compareIndividual(constraintsFile, constraintsFile)


  def compareSingleExpr(line: String, relationship: String, testExpr: FeatureExpr, isGroup: Boolean) = {
    var outputLine = line + "," + relationship + ","
    val testFeatures = testExpr.collectDistinctFeatures

    var found = false

    //test for implication in each of the four models and alter output line accordingly


    if (testErrors) {
      //System.err.println("in test errors")
      if (testFeatures.subsetOf(errorsFeatures)) {
        found = true
      }
    }


    if (testParser) {
      //System.err.println("in test parser")
      if (testFeatures.subsetOf(parserFeatures)) {
        found = true
      }
    }


    if (testType) {
      //System.err.println("in test type")
      if (testFeatures.subsetOf(typeFeatures)) {
        found = true
      }
    }


    if (testDefUse) {
      //System.err.println("in test defuse")
      if (testFeatures.subsetOf(defUseFeatures)) {
        found = true
      }
    }




    if (testNested) {
      //System.err.println("in test nested")
      if (testFeatures.subsetOf(nestedFeatures)) {
       found = true
      }
    }


    if (testFilePc) {
      if (testFeatures.subsetOf(filePcFeatures)){
        found = true
      }
    }


    //update counts
    if (found) {
      println(line)
    }
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

  def compareIndividual(relation: String, fileToCompare: String, isGroup: Boolean = false) = {
    resetCounts()
    System.err.println("==================================================")
    if (!new java.io.File(fileToCompare).exists) {
      System.err.println(fileToCompare + " does not exist")
    } else {

      val featureExprParser = new FeatureExprParser()
      var countTaut = 0
      var countContra = 0
      for (line <- Source.fromFile(fileToCompare).getLines()) {

        val testExpr = featureExprParser.parse(line)
        countAll += 1
      //  System.err.println("Checking constraint: " + countAll)
        if (!testExpr.isTautology()) {
          if (!testExpr.isContradiction()) {
            compareSingleExpr(line, relation, testExpr, isGroup)
          }
          else {
            System.err.println("CONTRADICTION: " + testExpr)
            countContra += 1
          }

        } else {
          System.err.println("TAUTOLOGY: " + testExpr)
          countTaut += 1
        }

      }

      System.err.println("total " + relation + " constraints in feature model: " + countAll)
      System.err.println("number of " + relation + " tautology expressions in feature model: " + countTaut)
      System.err.println("number of " + relation + " contradictory expressions in feature model: " + countContra)
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


  def getSolver(fm: SATFeatureModel): MySolver = {
    val solver = SolverFactory.newDefault()

    fm.variables.values foreach {
      k =>
        solver.registerLiteral(k)
    }
    solver.addAllClauses(fm.clauses)
    new MySolver(solver)
  }

  class MySolver(val solver: ISolver) {
    def implication(a: Int, b: Int) = !isSatisfiable(List(a, -b))

    def isSatisfiable(assumption: Iterable[Int]) =
      solver.isSatisfiable(new VecInt(assumption.toArray))

  }

}

