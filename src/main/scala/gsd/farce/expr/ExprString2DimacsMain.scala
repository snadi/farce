package gsd.farce.expr

import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import java.io.FileInputStream
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import gsd.linux._
import de.fosd.typechef.featureexpr.sat._
import de.fosd.typechef.featureexpr.sat.Or
import de.fosd.typechef.featureexpr.sat.And
import de.fosd.typechef.featureexpr.sat.Not
import gsd.linux.BNot
import org.kiama.rewriting.Rewriter._
import gsd.farce.features.model._
import gsd.linux.BId
import gsd.farce.features.model.FTrue
import gsd.linux.BNot
import gsd.farce.features.model.FFalse
import gsd.farce.features.model.FOr

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 25.07.13
 * Time: 22:29
 * To change this template use File | Settings | File Templates.
 */
object ExprString2DimacsMain {

  def main( args: Array[String] ){

    val input = new ANTLRInputStream( new FileInputStream( "src/test/resources/constraint_1.txt" ) )
    val lexer = new FExprLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new FExprParser(tokens)
    val output = parser.fexpr().value.asInstanceOf[SATFeatureExpr]
    println( typeChefExpr2FExpr( output ) )

//    val astSize = collectl{
//
//    }

  }

  def typeChefExpr2FExpr( e: SATFeatureExpr ): FExpr = {
    if( e.isInstanceOf[Or] )
//      e.asInstanceOf[Or].clauses.reduceLeft( BFalse:BExpr )( _ | typeChefExpr2BExpr(_) )
      if( e.asInstanceOf[Or].clauses isEmpty )
        FFalse()
      else
//        FOr( e.asInstanceOf[Or].clauses.map( typeChefExpr2FExpr ) )
        ( (FFalse():FExpr) /: e.asInstanceOf[Or].clauses )( _ | typeChefExpr2FExpr(_) )
    else if( e.isInstanceOf[And] )
      if( e.asInstanceOf[And].clauses isEmpty )
        FTrue()
      else
//        FAnd( e.asInstanceOf[And].clauses.map( typeChefExpr2FExpr ) )
        ( (FTrue():FExpr) /: e.asInstanceOf[And].clauses )( _ & typeChefExpr2FExpr(_) )
    else if( e.isInstanceOf[Not])
      FNot( typeChefExpr2FExpr( e ) )
    else if( e.isInstanceOf[DefinedExternal] )
      FId( e.asInstanceOf[DefinedExternal].feature )
    else
      sys.error( "Unknown node: " + e )
//      (e.asInstanceOf[Or]).clauses.foldLeft( BFalse:BExpr )( (a,b) => ( a | typeChefExpr2BExpr(b) ) )
//      println ( (e.asInstanceOf[Or]).clauses.foldLeft( BFalse:BExpr )( (a,b) => ( a | typeChefExpr2BExpr(b) ) ) )
//      ( BFalse:BExpr /: ( (e.asInstanceOf[Or]).clauses ) )( (a,b) => ( a | typeChefExpr2BExpr(b) ) )

  }




}
