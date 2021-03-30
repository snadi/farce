package gsd.farce.features.model.translation

import util.logging.{ConsoleLogger, Logged}
import gsd.linux._
import gsd.farce.features.model.translation.TOr
import gsd.linux.Id
import gsd.farce.features.model.translation.TNot
import gsd.farce.features.model.translation.TAnd
import gsd.farce.features.model.translation.TEq
import gsd.farce.features.model.translation.TId


object FarceTExpr extends Logged with ConsoleLogger {

  /**
   * @param eqMap map from an eq / neq expression as a (name, expr) pair to an identifier.
   *                used to substitute and expression to an identifier to support equality /
   *                inequality between string configs.
   * @param in expression to translate
   * @return
   */
  def toTExpr(eqMap: Map[(String, KExpr), String])(in: KExpr): TExpr = {
    def t(e: KExpr): TExpr = e match {
      case Id(x) => TId(x)

      case No  => TNo
      case Mod => TMod
      case Yes => TYes

      case And(x, y) => t(x) & t(y)
      case Or(x, y) => t(x) | t(y)
      case Not(e) => !t(e)


      case Eq(Id(name), value) if eqMap.contains(name, value) =>
        TId(eqMap((name, value)))

      case Eq(value, Id(name)) if eqMap.contains(name, value) =>
        TId(eqMap((name, value)))

      case NEq(Id(name), value) if eqMap.contains(name, value) =>
        !TId(eqMap((name, value)))

      case NEq(value, Id(name)) if eqMap.contains(name, value) =>
        !TId(eqMap((name, value)))

      case Eq(Id(x), Literal("")) => TEq(TId(x), TNo)

      // FIXME
      // X != "something"
      // can't handle this until we have proper literal support
      case NEq(Id(x),Literal(_)) => TYes
      case NEq(Id(x),KInt(_)) => TYes
      case NEq(Id(x),KHex(_)) => TYes

      case Eq(Id(x),Literal(_)) => !TEq(TId(x), TNo)
      case Eq(Id(x),KInt(_)) => !TEq(TId(x), TNo)
      case Eq(Id(x),KHex(_)) => !TEq(TId(x), TNo)

      case Literal("") => //FIXME ?
        log("WARN: Literal=\"\" not handled, returning TNo: " + in)
        TNo

      case Literal(_) | KHex(_) | KInt(_) => // FIXME?
        log("WARN: Literal / Hex / Int not handled, returning TYes: " + in)
        TYes

      case Eq(l, r) => TEq(t(l), t(r))
      case NEq(l, r) => !TEq(t(l), t(r))

      case e => sys.error("Unexpected expression (is it a boolean op?): " + e + ": " + e.getClass)
    }
    t(in)
  }

}

/**
 * A TExpr represent a tristate expression with three values.
 */
abstract class TExpr {


  // Converts the tristate expression to a boolean expression of 2 variables
  def toBExpr: (BExpr, BExpr)

  /**
   * Returns a single boolean expression.
   */
  def beq(other: TExpr): BExpr = other match {

    case TNo => toBExpr match {
      case (e1, e2) => !e1 & !e2
    }

    case TMod => toBExpr match {
      case (e1, e2) => e1 & !e2
    }

    case TYes => toBExpr match {
      case (e1, e2) => e1 & e2
    }

    case _ => (toBExpr, other.toBExpr) match {
      case ((l1, l2), (r1, r2)) => (l1 iff r1) & (l2 iff r2)
    }
  }

  /**
   * Returns a single boolean expression.
   */
  def <=(other: TExpr): BExpr = (this, other) match {
    
    case (TNo,_)  => BTrue
    case (TYes,_) => other beq TYes

    case (_,TYes) => BTrue
    case (_,TNo)  => this beq TNo

    case _ => (toBExpr, other.toBExpr) match {

        // x <= Mod
        case ((l1, l2), (BTrue,BFalse)) => !l2

        //Mod <= x
        case ((BTrue, BFalse),(r1, r2)) => r1

        case ((l1, l2), (r1, r2)) =>
          // Before simplifying:
          // (!(l1 | l2 ) implies (r1 | r2)) & !(l1 & l2 & r1 & !r2)
          (!l1 | r1 | r2) & (!l2 | r1 | r2 ) & (!l1 | !l2 | !r1 | r2)
      }
  }

  /**
   * Returns a single boolean expression.
   */
  def >(other: TExpr): BExpr = (this, other) match {
    
    case (TMod, TId(_)) => this beq TNo
    case (TId(_), TMod) => this beq TYes

    case _ => (toBExpr, other.toBExpr) match {
        //Yes > x
        case ((BTrue, BTrue), (r1, r2)) => !r2

        //x > No
        case ((l1, l2), (BFalse, BFalse)) => l1

        //FIXME
        case ((l1, l2), (r1, r2)) =>
          l1 & !r2 & (!l1 | l2 | !r1 | r2)
      }
  }

  def teq(other: TExpr): TExpr =
    TEq(this, other)

  def &(other: TExpr): TExpr = TAnd(this, other)
  def |(other: TExpr): TExpr = TOr(this, other)
  def unary_! = TNot(this)
}

case object TYes extends TExpr {
  def toBExpr = (BTrue, BTrue) //(1,1)
}
case object TMod extends TExpr {
  def toBExpr = (BTrue, BFalse) //(1,0)
}
case object TNo extends TExpr {
  def toBExpr = (BFalse, BFalse) //(0,0)
}

case class TId(v: String) extends TExpr {
  var tristate = false
  def toBExpr = if( tristate )
    (BId(v), BId(v + "_m"))
  else
    (BId(v), BTrue)
}

case class TAnd(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 & r1, l2 & r2)
  }
}

case class TOr(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 | r1, l2 | r2)
  }
}

case class TNot(e: TExpr) extends TExpr {
  def toBExpr = e.toBExpr match {
    case (l, r) => (!r, !l)
  }
}

case class TEq(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 iff r1, l2 iff r2)
  }
}

