package gsd.farce.utilities

import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.PropertyKeys._
import io.Source
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import java.util.Properties
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 01/10/13
 * Time: 8:34 AM
 * To change this template use File | Settings | File Templates.
 */
object ConflictChecker extends App{

  val properties = new Properties()
  loadPropertiesFile(args(0), properties)
  //Def Use
  var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  val defUseExpr = parser.fexpr().value
  val defUseFeatures = defUseExpr.collectDistinctFeatures
  System.err.println("INFO: Read defuse")


  val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
  var filePcExpr: FeatureExpr = FeatureExprFactory.True
  var filePcFeatures = Set[String]()
  if (!lines.isEmpty) {
    input = new ANTLRInputStream(lines.next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
    filePcFeatures = filePcExpr.collectDistinctFeatures
  }


  // if (filePcExpr != null)
  //  assert(!filePcExpr.isContradiction(), "Def Use Expression is a contradiction")
  System.err.println("INFO: Read filePc")
  if (filePcExpr == null) {
    System.err.println("INFO: No file pc ")
  } else if (filePcExpr.isTautology()) {
    System.err.println("INFO: File pc is tautology, will not check against it ")
  }

  //Parser
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  var parserExpr = parser.fexpr().value
  val parserFeatures = parserExpr.collectDistinctFeatures
  System.err.println("INFO: read parser")



  //Preprocessor errors
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  val errorsExpr = parser.fexpr().value
  val errorsFeatures = errorsExpr.collectDistinctFeatures
  System.err.println("INFO: read preproc errors")


  //type system
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(TYPE_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  var typeExpr = parser.fexpr().value//featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
  val typeFeatures = typeExpr.collectDistinctFeatures
  System.err.println("INFO: read type")

  //Nested ifdef
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)).getLines().next().trim)
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  val nestedExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
  System.err.println("INFO: parsed nesting expression")
  val nestedFeatures = nestedExpr.collectDistinctFeatures

  val combinedFormula = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr).and(nestedExpr).and(filePcExpr)

  if (combinedFormula.isSatisfiable()){
    System.err.println("ITS SATISFIABLE")
  }                                    else{
    System.err.println("ITS NOT")

    if(errorsExpr.and(parserExpr).isContradiction())
      println("Preprocessor and parser")
    else if (errorsExpr.and(typeExpr).isContradiction())
      println("Preprocessor and type")
    else if (errorsExpr.and(defUseExpr).isContradiction())
      println("Preprocessor and linker")
    else if (parserExpr.and(typeExpr).isContradiction())
      println("parser and type")
    else if(parserExpr.and(defUseExpr).isContradiction())
      println("parser and linker")
    else if (typeExpr.and(defUseExpr).isContradiction()) {
      println("type and linker")
                          var expr = defUseExpr
      var foundConflict = false
      for(line <- Source.fromFile(properties.getProperty(TYPE_CONSTRAINTS_FILE)).getLines()){
        if (!foundConflict){
        input = new ANTLRInputStream(line.trim())
        lexer = new FExprLexer(input)
        tokens = new CommonTokenStream(lexer)
        parser = new FExprParser(tokens)
        val constraint = parser.fexpr().value

        expr = expr.and(constraint)
        if (expr.isContradiction()){
          foundConflict = true
          println("CONFLICT WITH constraint:"+ constraint)
        }
        }

      }
    }
  }

}
