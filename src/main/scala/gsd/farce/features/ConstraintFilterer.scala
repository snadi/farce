package gsd.farce.features

import io.Source
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 11/09/13
 * Time: 2:49 PM
 * To change this template use File | Settings | File Templates.
 */
object ConstraintFilterer {


  def main(args: Array[String]) {
    val inputLines = io.Source.fromFile(args(0)).getLines().toList
    val FILTER_SIZE = args(1).toInt



    for (file <- inputLines){
      val line = Source.fromFile(file).getLines().next().trim

      val input = new ANTLRInputStream(line.trim())
      val lexer = new FExprLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new FExprParser(tokens)
      val constraint = parser.fexpr().value

      if (constraint.collectDistinctFeatures.size <= FILTER_SIZE){
        println(file)
      } else{
        System.err.println("Filter: " + file + " with " + constraint.collectDistinctFeatures.size + " variables")
      }
    }
  }

}
