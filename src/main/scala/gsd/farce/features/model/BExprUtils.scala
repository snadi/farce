package gsd.farce.features.model

import org.kiama.rewriting.Rewriter._
import gsd.linux._
import de.fosd.typechef.featureexpr.sat._
import de.fosd.typechef.featureexpr.sat.Or
import de.fosd.typechef.featureexpr.sat.And
import gsd.farce.features.model.FNot
import de.fosd.typechef.featureexpr.sat.Not
import gsd.farce.features.model.FTrue
import gsd.farce.features.model.FId
import gsd.farce.features.model.FFalse
import de.fosd.typechef.featureexpr.FeatureExpr
import gsd.linux.BId
import gsd.linux.BNot
import gsd.linux.BImplies
import gsd.linux.BOr
import gsd.linux.BAnd
import org.kiama.rewriting.Rewriter

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 11.07.13
 * Time: 20:03
 * To change this template use File | Settings | File Templates.
 */
object BExprUtils {

  def makeConjunction( expressions: Seq[BExpr] ) = ( (BTrue:BExpr) /: expressions)( _ & _ )

  def splitConjunction( e: BExpr ): List[BExpr] = e match{
    case BAnd( l, r ) => splitConjunction( l ) ::: splitConjunction( r )
    case a => a :: Nil
  }

  def toTypeChefExpr( bexpr: BExpr ): FeatureExpr = bexpr match{
    case BId( n ) => SATFeatureExprFactory.createDefinedExternal( n )
    case BNot( e ) => toTypeChefExpr( e ).not()
    case BAnd( l, r ) => toTypeChefExpr( l ).and( toTypeChefExpr( r ))
    case BOr( l, r ) => toTypeChefExpr( l ).or( toTypeChefExpr( r ))
    case BImplies( l, r ) => toTypeChefExpr( l ).implies( toTypeChefExpr( r ))
    case BTrue => SATFeatureExprFactory.True
    case BFalse => SATFeatureExprFactory.False
    case e => sys.error( "Not supported yet: " + e )
  }

  def simplify( exprs: List[BExpr] ) = {
    Rewriter.rewrite( simplifyRule )( exprs )
  }

  def simplifyAggressive( exprs: List[BExpr] ) = {
    splitConjunction(
      Rewriter.rewrite( simplifyRule )( makeConjunction( exprs.filterNot( _ == BTrue ) ) )
    ).distinct
  }

  val simplifyRule = innermost{
    rule{
      case BNot( BNot( a ) )        => a
      case BNot( BTrue )               => BFalse
      case BNot( BFalse )              => BTrue
      case BOr( a, b ) if a == b             => a
      case BOr( BTrue, a )                  => BTrue
      case BOr( BFalse, a )                 => a
      case BOr( a, BTrue )                  => BTrue
      case BOr( a, BFalse )                 => a
      case BAnd( BTrue, a )                 => a
      case BAnd( BFalse, a )                => BFalse
      case BAnd( a, BTrue )                 => a
      case BAnd( a, BFalse )                 => BFalse
      case BAnd( a, b ) if a == b            => a
      case BAnd( BAnd( a, b ), c ) if b == c  => BAnd( a, b )
      case BAnd( a, BAnd( b , c )) if a == b  => BAnd( b, c )

      case BOr(BAnd(a,b),BAnd(c,d)) if a == c  => BAnd( a, BOr( b, d ) )
      case BOr(BAnd(a,b),BAnd(c,d)) if b == d  => BAnd( b, BOr( a, c ) )
      case BOr(BAnd(a,b),BAnd(c,d)) if b == c  => BAnd( b, BOr( a, d ) )
      case BOr(BAnd(a,b),BAnd(c,d)) if a == d  => BAnd( a, BOr( b, c ) )

      case BAnd(BOr(a,b),BOr(c,d))  if a == c  => BOr( a, BAnd( b, d ) )
      case BAnd(BOr(a,b),BOr(c,d))  if b == d  => BOr( b, BAnd( a, c ) )
      case BAnd(BOr(a,b),BOr(c,d))  if b == c  => BOr( b, BAnd( a, d ) )
      case BAnd(BOr(a,b),BOr(c,d))  if a == d  => BOr( a, BAnd( b, c ) )

      case BOr( a, BAnd( b, c ) ) if ( a == b || a == c)   => a
      case BOr( BAnd( a, b ), c ) if ( a == c || b == c )  => c

      case BOr( BAnd( a, b ), BOr( c, d ) ) if ( a == c || a == d || b == c || b == d )   => BOr( c, d )
      case BOr( BOr( c, d ), BAnd( a, b ) ) if ( a == c || a == d || b == c || b == d )   => BOr( c, d )

      case BOr( BNot( a ), b ) if a == b => BTrue
      case BOr( a, BNot( b ) ) if a == b => BTrue
//      case BOr( a, b ) if( )


      case BImplies( a, BTrue )             => BTrue
      case BImplies( a, b ) if a == b        => BTrue
      case BImplies( a, BAnd(b,c) ) if a == b => BImplies( a, c )
      case BImplies( a, BAnd(b,c) ) if a == c => BImplies( a, b )
      case BImplies( a, BOr(b,c) ) if a == b  => BTrue
      case BImplies( a, BOr(b,c) ) if a == c  => BTrue
      case BImplies( BAnd(a,b), c ) if ( a == c || b == c ) => BTrue
      case BImplies( BFalse, a )            => BTrue
      case BImplies( a, BFalse )            => BNot( a )
      case BImplies( BTrue, a )             => a

      // just curious if that helps a bit:
      // at least we need it later, since the C expression parser cannot deal
      // with implications (not part of C specification afaik)
      case BImplies( a, b )                  => BOr( BNot(a), b )
    }
  }

}
