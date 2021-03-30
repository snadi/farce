package gsd.farce.utilities

import de.fosd.typechef.featureexpr.FeatureExpr
import java.io.{File, FileWriter, PrintWriter}
import java.util.Properties
import gsd.farce.utilities.Utilities._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import scala.io.Source
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.features.CreateDimacs
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr

/**
 * Created by snadi on 19/02/15.
 */
/*
Reads all separate code formulas and combines them into one code formuala and outputs it
 */
object FullCodeFormulaCreator {
  var defUseExpr, parserExpr, errorsExpr, nestedExpr, typeExpr, bigConjunction, spec1, spec2, filePcExpr: FeatureExpr = null

  val properties = new Properties()

  def getConstraintType(input: String) = {
    var constraintType = ""
    if (input.contains("/")) {
      constraintType = input.substring(input.lastIndexOf("/") + 1)
    }

    constraintType = constraintType.substring(0, constraintType.indexOf(".constraints"))

    constraintType
  }

  def main(args: Array[String]) {

    loadPropertiesFile(args(0), properties)
    val config = getSystemConfig(args(0))

    readFormulas
    System.out.println("read all formulas")

    writeFormula(bigConjunction, "output/FullCode/fullCodeFormula", config)
    writeFormula(spec1, "output/FullCode/spec1", config)
    writeFormula(spec2, "output/FullCode/spec2", config)



  }

  def writeFormula(formula: FeatureExpr, fileName: String, config: Config){
    val formulaWriter = new PrintWriter(new FileWriter(new File(fileName + ".txt")))
    bigConjunction.print(formulaWriter)
    formulaWriter.println()
    formulaWriter.close()

    CreateDimacs.createDimacs(formula.asInstanceOf[SATFeatureExpr], fileName + ".dimacs", false, config.getPrefix, config.getSuffix)

  }

  def readFormulas {
    //get feature expressions for all code formulas

    //Def Use
    var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
    var lexer = new FExprLexer(input)
    var tokens = new CommonTokenStream(lexer)
    var parser = new FExprParser(tokens)
    defUseExpr = parser.fexpr().value
    System.out.println("INFO: Read defuse")


    val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
    if (!lines.isEmpty) {
      input = new ANTLRInputStream(lines.next().trim())
      lexer = new FExprLexer(input)
      tokens = new CommonTokenStream(lexer)
      parser = new FExprParser(tokens)
      filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
    }

    System.out.println("INFO: Read filePc")
    if (filePcExpr == null) {
      System.out.println("INFO: No file pc ")
    } else if (filePcExpr.isTautology()) {
      System.out.println("INFO: File pc is tautology, will not check against it ")
    }

    //Parser
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    parserExpr = parser.fexpr().value
    System.out.println("INFO: read parser")



    //Preprocessor errors
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    errorsExpr = parser.fexpr().value
    System.out.println("INFO: read preproc errors")


    //type system
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(TYPE_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    typeExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
    System.out.println("INFO: read type")

    //Nested ifdef
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)).getLines().next().trim)
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    nestedExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
    System.out.println("INFO: parsed nesting expression")

    spec1 = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr)
    spec2 = nestedExpr.and(filePcExpr)

    bigConjunction = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr).and(nestedExpr)
    System.out.println("created formula")
    if (bigConjunction.isTautology()) {
      System.out.println("WARNING: Big conjunction is a tautology! No need for comparison")

    } else if (bigConjunction.isContradiction()) {
      System.out.println("WARNING:Big conjunction is a contradiction! will not check against it")
    }
  }


}
