package gsd.farce

import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExpr, FeatureExprFactory, FeatureExprParser}
import features.FeatureUtils._
import io.Source
import de.fosd.typechef.featureexpr.sat.{SATFeatureExprFactory, SATFeatureModel}
import java.util.Properties
import gsd.farce.utilities.Utilities
import gsd.farce.utilities.PropertyKeys._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.Utilities._
import gsd.farce.features.DimacsComposer

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 10/05/13
 * Time: 4:52 PM
 * To change this template use File | Settings | File Templates.
 */
object ConflictTest extends App {

  var parserExpr, errorsExpr, filePcExpr, nestedExpr, spec2, defUseExpr, typeExpr, spec1, bigConjunction: FeatureExpr = null

  readFormulas
//  println("starting")
//  val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
////  val nestedifdefExpr = featureExprParser.parseFile("../CaseStudies/farce-BusyBoxAnalysis/output/NestedIfdefs/nestedIfDefFormula.txt")
//  // val nestedifdefExpr = featureExprParser.parseFile("../CaseStudies/farce-BusyBoxAnalysis/output/NestedIfdefs/nestedIfDefCombinedImplications.txt")
//  //println("read expression")
//  //val errors = Array("def(CONFIG_FEATURE_LAST_SMALL) => def(CONFIG_FEATURE_UTMP)", "def(CONFIG_MAKEDEVS) => (def(CONFIG_FEATURE_MAKEDEVS_TABLE)|def(CONFIG_FEATURE_MAKEDEVS_LEAF))","def(CONFIG_ASH) => !def(CONFIG_NOMMU)")
//
//  var parserExpr = Utilities.parseConstraint(Source.fromFile("../CaseStudies/farce-uClibc/output/Linker/defUseFormula.txt").getLines().next())
//
//  //println(nestedifdefExpr.and(FeatureExprFactory.createDefinedExternal("CONFIG_ASH")).and(FeatureExprFactory.createDefinedExternal("CONFIG_NOMMU").not()).isContradiction())
//  var break = false
//  for (typeConstraint <- Source.fromFile("../CaseStudies/farce-uClibc/output/TypeErrors/typeErrorConstraints.txt").getLines()) {
//    if (!break) {
//      val expr = Utilities.parseConstraint(typeConstraint)
//      parserExpr = parserExpr.and(expr)
//      if (parserExpr.isContradiction()) {
//        println("prob: " + typeConstraint)
//        break = true
//      } else {
//        println("passed: " + typeConstraint)
//      }
//    }
//  }
def readFormulas {
  //get feature expressions for all code formulas
  val properties = new Properties()
  loadPropertiesFile("linux", properties)
  //Def Use
  var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
  var lexer = new FExprLexer(input)
  var tokens = new CommonTokenStream(lexer)
  var parser = new FExprParser(tokens)
  defUseExpr = parser.fexpr().value
  println("read defuse")


  val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
  if (!lines.isEmpty) {
    input = new ANTLRInputStream(lines.next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
  }
  println("Read pc")


  //Parser
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  parserExpr = parser.fexpr().value
               println("read parser)")


  //Preprocessor errors
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  errorsExpr = parser.fexpr().value
            println("read pperror")

  //type system
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(TYPE_FORMULA_FILE)).getLines().next().trim())
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  typeExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
                          println("read type")

  //Nested ifdef
  input = new ANTLRInputStream(Source.fromFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)).getLines().next().trim)
  lexer = new FExprLexer(input)
  tokens = new CommonTokenStream(lexer)
  parser = new FExprParser(tokens)
  nestedExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
                     println("Read nested")


  var test = errorsExpr.and(parserExpr)

  if(test.isContradiction()){
    println("preprocessor and parser")
  }else{
     test = test.and(typeExpr)

    if(test.isContradiction()){
      println("preprocessor, parser, and type")
    }else{
      test = test.and(defUseExpr)
      if(test.isContradiction()){
        println("iT'S THE LINKER")
      } else{
        test= test.and(nestedExpr)

        if(test.isContradiction()){
          println("nested causes prob")
        }
      }
    }
  }

//    spec1 = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr)
//
//    spec2 = nestedExpr.and(filePcExpr)
//    bigConjunction = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr).and(nestedExpr)


}
}
