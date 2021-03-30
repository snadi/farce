package gsd.farce.utilities

import de.fosd.typechef.featureexpr.FeatureExprFactory
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import java.io.{File, FileWriter, PrintWriter}
import gsd.farce.features.FeatureUtils._
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 04/09/13
 * Time: 10:52 AM
 * To change this template use File | Settings | File Templates.
 */
/*
takes an input file which contains a list of constraints (Each constraint on a line)
creates a conjunction of all those constraints
takes an output file to which to print the formula
 */
object FormulaCreator {

  def main(args: Array[String]) {
    val constraints = io.Source.fromFile(args(0)).getLines().toList
    val formulaWriter = new PrintWriter(new FileWriter(new File(args(1))))

    var formula = FeatureExprFactory.True

    for (constraintFile <- constraints) {
      val lines = io.Source.fromFile(constraintFile).getLines()
      for (line <- lines) {
        val input = new ANTLRInputStream(line.trim())
        val lexer = new FExprLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new FExprParser(tokens)
        val constraint = parser.fexpr().value


        formula = formula.and(constraint)

        if(formula.isContradiction){
          println(constraintFile + " created a contradiction")
        }
      }
    }

    formula.print(formulaWriter)
    formulaWriter.println()

    formulaWriter.close()
  }
}
