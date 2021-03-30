package gsd.farce.features.model

import gsd.linux._
import org.kiama.rewriting.Rewriter._
import gsd.linux.Or
import gsd.farce.features.model.FAnd
import gsd.linux.And
import gsd.farce.features.model.FOr
import gsd.farce.features.model.FGreaterThan

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 03.05.13
 * Time: 12:54
 * To change this template use File | Settings | File Templates.
 */

object FExpr{

/*  def toFExpr( e: KExpr ): FExpr = {
    val toFExpr = everywheretd{
      rule{
        case And(l,r) => FAnd(l,r)
        case Or(l,r) => FOr(l,r)
        case Eq(l,r) => FEq(l,r)
        case Not(e) => FNot(e)
      }}
    rewrite( toFExpr )(e).asInstanceOf[FExpr]
  }*/


}

abstract class FExpr {

  def >( o: FExpr ): FExpr = FGreaterThan(this, o)
  def implies( o: FExpr ): FExpr = FImplies(this, o)
  def |(o: FExpr): FExpr = FOr(this, o)
  def &(o: FExpr): FExpr = FAnd(this, o)
  def iff(o: FExpr): FExpr = FIff(this, o)

  def unary_!(): FExpr = FNot( this )

}

case class FGreaterThan( left: FExpr, right: FExpr ) extends FExpr
case class FAnd( left: FExpr, right: FExpr ) extends FExpr
case class FOr( left: FExpr, right: FExpr ) extends FExpr
case class FImplies( left: FExpr, right: FExpr ) extends FExpr
case class FIff( left: FExpr, right: FExpr ) extends FExpr
case class FEq( left: FExpr, right: FExpr ) extends FExpr

case class FNot( e: FExpr ) extends FExpr

case class FId( id: String ) extends FExpr

case class TristateValue( v: String ) extends FExpr

case class FTrue() extends FExpr{
  override def & (other: FExpr) = other
  override def implies (other: FExpr) = other
  override def toString = "TRUE"
}
case class FFalse() extends FExpr{
  override def | (other : FExpr) = other
  override def toString = "FALSE"
}