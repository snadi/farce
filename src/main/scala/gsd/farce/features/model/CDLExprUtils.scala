package gsd.farce.features.model

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.sat.SATFeatureExprFactory
import gsd.cdl.model._

/**
 * Created by berger on 22/02/14.
 */
object CDLExprUtils {

  def makeConjunction( expressions: Seq[CDLExpression] ) = ( (True():CDLExpression) /: expressions)( _ & _ )

  def toTypeChefExpr( cdlexpr: CDLExpression ): FeatureExpr = cdlexpr match{
    case Identifier( n ) => SATFeatureExprFactory.createDefinedExternal( n )
    case Not( e ) => toTypeChefExpr( e ).not()
    case And( l, r ) => toTypeChefExpr( l ).and( toTypeChefExpr( r ))
    case Or( l, r ) => toTypeChefExpr( l ).or( toTypeChefExpr( r ))
    case Implies( l, r ) => toTypeChefExpr( l ).implies( toTypeChefExpr( r ))
    case True() => SATFeatureExprFactory.True
    case False() => SATFeatureExprFactory.False
    case LongIntLiteral( 0 ) => SATFeatureExprFactory.False
    case LongIntLiteral( 1 ) => SATFeatureExprFactory.True
    case e => sys.error( "Not supported yet: " + e )
  }

  def isBooleanExpression( e: CDLExpression ):Boolean = e match{
    case Identifier(_) => true
    case True() => true
    case False() => true
    case LongIntLiteral( 0 ) => true
    case LongIntLiteral( 1 ) => true
    case Not( e ) => isBooleanExpression( e )
    case And( a, b ) => isBooleanExpression( a ) && isBooleanExpression( b )
    case Or( a, b ) => isBooleanExpression( a ) && isBooleanExpression( b )
    case Implies( a, b ) => isBooleanExpression( a ) && isBooleanExpression( b )
    case _ => false
  }

}
