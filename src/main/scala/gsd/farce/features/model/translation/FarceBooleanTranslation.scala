/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.farce.features.model.translation

import org.kiama.rewriting.Rewriter._
import java.io.PrintStream
import gsd.linux._
import gsd.farce.features.model.BExprUtils

/**
 * Builds the Boolean formula for an abstract Kconfig model.
 *
 * The translation is (or at least tries to be) a less constrained version of the
 * true Linux Kconfig semantics. The idea is that if inputted into a SAT solver,
 * any valid configuration of Linux will be satisfiable with the generated
 * formula. However, the inverse is not true; Some formulas that are satisfiable
 * with the generated formula may not be a valid configuration.
 *
 * TODO this should be replaced by TristateSTrnaslation in the fm-translation project.
 * TODO inherited expression is not used
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object FarceBooleanTranslation extends KExprList with BExprList with ExprRewriter {

  case class BTrans(exprs: List[BExpr], genVars: List[String])


  /**
   * Translates a KExpr to a propositional BExpr. This is a total mapping
   * from KExpr to BExpr.
   */
  def toBExpr(in: KExpr) = {
    def translate(e: KExpr): BExpr = e match {
      case No => BFalse
      case Mod | Yes => BTrue

      case Literal(_) | KHex(_) | KInt(_) => BFalse

      case Eq(Id(n), Yes) => BId(n)
      case Eq(Id(n), Mod) => BId(n)
      case Eq(Id(n), No) => BNot(BId(n))
      case Eq(Id(n), Literal("")) => BNot(BId(n))
      case Eq(Id(n), Literal(_)) => BId(n)
      case Eq(Id(n), KHex(_)) => BId(n)
      case Eq(Id(n), KInt(_)) => BId(n)
      case Eq(Id(x), Id(y)) => BIff(BId(x), BId(y))

      case NEq(Id(n), Yes) => BTrue //TODO Check this
      case NEq(Id(n), Mod) => BTrue //TODO Check this
      case NEq(x, y) => BNot(translate(Eq(x, y)))

      case And(x, y) => BAnd(translate(x), translate(y))
      case Or(x, y) => BOr(translate(x), translate(y))

      case Not(e) => BNot(translate(e)) //TODO Check this, may make a constraint stronger
      case Id(n) => BId(n)

      case e => sys.error("Unexpected: " + e + ": " + e.getClass)
    }
    translate(in)
  }

  /**
   * Construct a list containing first i elements, the ith element is the
   * current default being processed, the 0 to i-1 elements are the previous
   * default conditions. Negate all previous default conditions and conjoin it
   * with the current default condition.
   *
   * TODO currently ignoring previous expressions on ADefault
   */
  def mkDefaults(defs: List[ADefault]): List[BExpr] = {

    /**
     * Chunks defaults such that each list of defaults has the same consecutive
     * default values (ie. Default.iv)
     */
    def chunkDefaults(defs: List[ADefault]): List[List[ADefault]] = defs match {
      case Nil => Nil
      case ADefault(iv, _, _) :: _ =>
        val (same, rest) = defs.partition {_.iv == iv}
        same :: chunkDefaults(rest)
    }

    def negateDefaults(prevs: List[ADefault]) =
      ((BTrue: BExpr) /: prevs) {(x, y) => x & !toBExpr(y.currCondition)}

    /**
     * Chunk defaults and reverse such that the last default chunk is first.
     * This is necessary since doChunks negates the tail, which is the first n-1
     * defaults when the list is reversed.
     *
     * For example, given defaults [X,Y,Z], the reverse is [Z,Y,X]. doChunks will
     * process Z first, with [Y,X] as the tail. Z is the last default, and thus,
     * must negate Y and X.
     */
    def doChunks(prevs: List[List[ADefault]])(acc: List[BExpr]): List[BExpr] =
      prevs match {
        case Nil => acc
        case lst :: rest =>
          val negatedPrev = negateDefaults(rest.flatten[ADefault])
          doChunks(rest) {

            //Remove defaults with value 'No' or Literal("")
            {
              lst filterNot
                { case ADefault(iv, _, _) => iv == No | iv == Literal("") } map
                {
                  case ADefault(Yes, _, c) => negatedPrev & toBExpr(c)
                  case ADefault(Mod, _, c) => negatedPrev & toBExpr(c)
                  case ADefault(v: Value, _, c)   => negatedPrev & toBExpr(c)
                  case ADefault(iv, _, c) => negatedPrev & toBExpr(c && iv)
                }
            } ++ acc
          }
      }

    doChunks(chunkDefaults(defs).reverse)(Nil)
  }

  /**
   * Determines when a KExpr, after translating to a BExpr, when used as an
   * antecedent, would be too constraining.
   */
  def isTooConstraining(e: KExpr): Boolean =
    count { case _ : Eq | _ : NEq => 1}(e) > 0

  def isTooConstraining(d: ADefault): Boolean =
    isTooConstraining(d.iv) || isTooConstraining(d.currCondition)


  def mkPresence(k: AbstractKConfig): BTrans = {

    /**
     * A container for a list of expressions and a list of generated
     * equivalences between the original expression and a generated variable.
     */
    case class EquivExprs(exprs: List[BExpr], equivs: List[BExpr])

    object IdGen {
      var i = 0
      def allIds = (1 to i).map { "x" + _ }.toList
      def next = { i+=1; "x" + i }
    }

    /**
     * Map from a sub-expression to a generated variable
     */
    val cache = new collection.mutable.HashMap[Set[BExpr], BId] {
      def apply(e: BExpr) =
        getOrElseUpdate(Set() ++ e.splitConjunctions, BId(IdGen.next))
    }

    /**
     * Replaces expressions with cached identifiers.
     */
    def replaceWithVars(lst: List[BExpr]): List[BExpr] = lst.map(cache.apply)

    /*
     * For each config, generate the presence expression and any equivalence
     * expressions generated by any new variables from the reverse dependencies.
     *
     * FIXME use modOnly
     */
    val exprs = k.configs.flatMap {
      case AConfig(_,name, _, inh, pro, defs, krevs, _, modOnly) =>
        val proE   = toBExpr(pro)

        //Reverse dependency and defaults
        val asAnte = ((krevs filterNot isTooConstraining) map toBExpr) :::
                mkDefaults(defs filterNot isTooConstraining)
        val asCons = krevs.map(toBExpr) ::: mkDefaults(defs)
        val numOfIds = identifiers(proE).size + identifiers(asCons).size


//        val ret = ( proE | (asAnte.mkDisjunction implies BId(name)) &
//          (BId(name) implies asCons.mkDisjunction) ) :: Nil

        //Heuristic for estimating size of resulting CNF translation
        val ret = //if (numOfIds < 40 && asCons.size < 5)
          (proE | (asAnte.mkDisjunction implies BId(name)) &
                  (BId(name) implies asCons.mkDisjunction)) :: Nil
//        else {
//          val equivs = asCons.map { e => cache(e) iff e }
//          (proE | (replaceWithVars(asAnte).mkDisjunction implies BId(name)) &
//                  (BId(name) implies replaceWithVars(asCons).mkDisjunction)) :: equivs
//        }

      ret
    }

    BTrans(exprs, IdGen.allIds)
  }


  def mkChoice(k: AbstractKConfig): List[BExpr] = k.choices.flatMap {
    case AChoice(vis, isBool, isMand, memIds) =>
      val xors = Combinations.choose(2, memIds).map {
        case fst :: snd :: Nil => !BId(fst) | !BId(snd)
        case _ => sys.error("This should never happen")
      }

      val disj = toBExpr(vis) implies (memIds.map(BId): List[BExpr]).reduceLeft(BOr)
      
      if (isMand) disj :: xors else xors
  }

  /**
   * Creates a BTrans for an AbstractKConfig.
   * BTrans is simply a wrapper for a boolean expression (BExpr) and a set of
   * generated variables.
   */
  def mkBooleanTranslation(k: AbstractKConfig) : BTrans = {
    val pres = mkPresence(k)
    BTrans( ( pres.exprs ::: mkChoice(k) ) distinct  , pres.genVars)
  }
}

object FarceBooleanTranslationMain {
  def main(args: Array[String]) {
    val k = KConfigParser.parseKConfigFile(args(0))
    val trans = FarceBooleanTranslation.mkBooleanTranslation(k.toAbstractKConfig)
    
    val out = new PrintStream(args(1))

    val ids = (trans.exprs flatMap (_.identifiers)).toSet --  trans.genVars
    
    for (id <- ids) out.println("@ " + id)
    for (id <- trans.genVars) out.println("$ " + id)
    for (e  <- BExprUtils.simplifyAggressive( trans.exprs ) ) out.println(e)
    
    out.close()
  }
}

trait ExprRewriter {
  def identifiers(e: BExpr): Set[String] = collects {
    case BId(n) => n
  }(e)

  def identifiers(es: Iterable[BExpr]): Set[String] = Set() ++ es.flatMap(identifiers)

}

