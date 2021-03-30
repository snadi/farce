package gsd.farce.utilities

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.features.CreateDimacs
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/07/13
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */
object ConstrFileToDimacsConverter {


  //read constraints from a file and output them as individual dimacs files
  //args(0): dir name to output to (output/<dirName>/DimacsConstraints
  def main(args: Array[String]) {

    val dirName = args(0)
    var formulaExpr = FeatureExprFactory.True

    val inputLines = io.Source.fromFile(args(1)).getLines().toList

    println("Num. of lines read: " + inputLines.size)

    var constraintNum = 1

    var constraints = Set[FeatureExpr]()

    for (line <- inputLines) {
      System.err.println("analyzing constraint: " + constraintNum)
      val input = new ANTLRInputStream(line.trim())
      val lexer = new FExprLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new FExprParser(tokens)
      val constraint = parser.fexpr().value

      if (!constraints.contains(constraint)) {
        val newFormula = constraint.and(formulaExpr)
        if (newFormula.isContradiction() || newFormula.isTautology()) {
          System.err.println("ERROR Constraint: " + constraint + " caused contradiction or tautology")
        } else {
          formulaExpr = newFormula
          System.err.println("WRITING constraint: " + constraintNum)
          CreateDimacs.createDimacs(constraint.asInstanceOf[SATFeatureExpr], "output/" + dirName + "/DimacsConstraints/constraint_" + constraintNum + ".dimacs", false, "", "")
          constraintNum += 1
        }

        constraints += constraint
      }
    }
  }

}
