//package gsd.farce.features.model
//
//import gsd.linux._
//import gsd.linux.BId
//import scala.Some
//import gsd.linux.AConfig
//import gsd.linux.ADefault
//import gsd.linux.TId
//import gsd.linux.AChoice
//import gsd.linux.Tseitin.IdGen
//
///**
//* Created with IntelliJ IDEA.
//* User: berger
//* Date: 03.05.13
//* Time: 12:18
//* To change this template use File | Settings | File Templates.
//*/
//object Kconfig2FormulaMain {
//
//
//
//}
//
//class FullTranslation(val k: AbstractKConfig, val addUndefined: Boolean = true) {
//
//  import TristateTranslation.MODULES_CONFIG_NAME
//  import FExpr._
//
//  val isModulesConfigDefined = k.findConfig(MODULES_CONFIG_NAME).isDefined
//
////  object IdGen {
////    val prefix = "_X"
////    var i = 0
////    def next = { i+=1; prefix + i }
////    def allIds = (1 to i).map { prefix + _ }.toList
////  }
//
////  def generated: List[String] =
////    IdGen.allIds ::: (IdGen.allIds map { _ + "_m"})
//
//  def identifiers: List[String] =
//    k.identifiers.toList// ::: IdGen.allIds
//
//  def size: Int = identifiers.size
//
//  /**
//   * Var i (odd) represents identifier x, Var i+1 (even) represents x_m
//   */
//  def idMap: Map[String, Int] =
//    Map() ++ {
//      (identifiers flatMap
//              { id => List(id, id + "_m") }).zipWithIndex map
//                     { case (id,i) => (id, i + 1) }
//    }
//
////  val literalMap: Map[(String, KExpr), String] =
////    (k.literalExprs map (_ -> IdGen.next)).toMap
//
//  /**
//   * function instantiated with a map from string literal to generated variable
//   */
////  val toTExpr: KExpr => TExpr = TExpr.toTExpr(literalMap)
//
//  /**
//   * Stateful: Changes identifiers in IdGen
//   */
//  lazy val translate: List[FExpr] =
//    ((k.configs flatMap translate) ::: (k.choices flatMap translate))// map
////      (_.simplify) filter (_ != BTrue)
//
//
//  def translate(c: AChoice): List[FExpr] = c match {
//    // Boolean, mandatory choice == XOR group
//    case AChoice(vis, true, isOpt, members) => {
//      // exclusions between all members
//      val exclusions = Combinations.choose(2, members) map {
//        case List(x,y) => !FId(x) | !FId(y)
//        case _ => sys.error("This should never occur since we use Combinations.choose(2,...)")
//      }
//
//      if( isOpt )
//        (( toFExpr(vis) > TristateValue( "no" ) ) implies members.map( FId(_): FExpr ).reduce(_|_) ) :: exclusions
//      else
//        exclusions
//
//    }
//    // any other kind of group doesn't impose a constraint
//    case _ => Nil
//  }
//
//  /**
//   * Always introducing new variable for reverse dependency expression.
//   */
//  def translate(c: AConfig): List[FExpr] = c match {
//
//    // TODO verify that environment variables shouldn't impose any constraint
//    case AConfig(_,name, _, _, _, _, _, _, _) if k.env contains name =>
//      Nil
//
//    case AConfig(nodeId, name, t, inh, pro, defs, rev, ranges, modOnly) =>
//
//      val rds = rev.map( FExpr.toFExpr )
//
////      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }
////
////      // rdsEquiv will be Some if a generated variable is used to represent
////      // entire reverse dependency expression
////      val (rdsId, rdsEquiv): (TId, BExpr) = {
////          val id = IdGen.next
////          (TId(id), rdsExpr beq TId(id))
////        }
//
//      // create default constraints
//      def defaults(rest: List[ADefault]): List[FExpr] = {
//
//        // uses a BId to represent the negated previous conditions
//        def traverse(rest: List[ADefault], prevCondId: FId): List[FExpr] = rest match {
//          case Nil => Nil
//
//            // FIXME not using prev on ADefault
//          case ADefault(e,_,cond)::tail =>
//
//            // generate condition id for next iteration of t(..)
//            //TODO optimization: not necessary if tail is empty
//            val (nextCondId, nextCondEquiv) = {
//              val id = IdGen.next
//              val (bid1, bid2) = (FId(id), FId(id + "_m"))
//
//              //FIXME id generator still generates _2 variable, so we make it dead
//              (bid1, (bid1 iff (prevCondId & (FExpr.toFExpr(cond) eq TristateValue("n")))) & !bid2)
//            }
//
//            // antecedent for current default
//            val ante = prevCondId & ( toFExpr(cond) > TristateValue("n") )
//
//            //
//            // Handle defaults differently for tristate and non-tristate configs
//            //
//            val defaultExpr = t match {
//              case KStringType | KIntType | KHexType =>
//                // FIXME check if an entry in the map exists
//                ante implies FId(literalMap((name, e)))
//
//              case  _ =>
//                // Handle default y quirk (i.e. if default y, then config takes
//                // value of its condition, not y)
//                val tex = if (e == Yes) toFExpr(cond) else toFExpr(e)
//
//                // tex | rdsId: config takes the max of the default or the RDs
//                ante implies (FId(name) eq (tex | rdsId))
//            }
//            defaultExpr ::
//              nextCondEquiv :: // default condition equivalence
//              traverse(tail, nextCondId)
//        }
//
//        // negated prompt condition for defaults (since prevCondId represents
//        // the negated previous condition)
//        val (proId, proEquiv) = {
//          val id = IdGen.next
//          val (bid1, bid2) = (BId(id), BId(id + "_m"))
//
//           //FIXME id generator still generates _2 variable, so we make it dead
//          (bid1, (bid1 iff (toTExpr(pro) beq TNo)) & !bid2)
//        }
//
//        // The prompt acts as the first negated condition
//        proEquiv :: traverse(rest, proId)
//      }
//
//      // Disallow mod state from Boolean, String, Int and Hex configs
//      val typeConstraint: Option[BExpr] = t match {
//        case KBoolType | KStringType | KIntType | KHexType =>
//          Some(BId(name) implies BId(name + "_m") )
//        case _ => None
//      }
//
//      // Disallow (0,1) state
//      val tristateConstraints: List[BExpr] =
//        AbstractKConfig.identifiers(c).toList map { id => BId(id) | !BId(id + "_m") }
//
//      // Make undefined variables dead referenced in this config
//      val undefinedConstraints: List[BExpr] =
//        if (addUndefined) ((AbstractKConfig.identifiers(c) -- (k.configs map (_.name)) -- k.env) map (!BId(_))).toList
//        else Nil
//
//      // an upper bound is only imposed on tristate configs, if it's parent is also tristate
//      // FIXME not used
//      val upperBoundConstraints: Option[BExpr] = (t, k.parentMap.get(c)) match {
//        case (KTriType, Some(AConfig(_,parentName,KTriType,_,_,_,_,_,_))) => Some(TId(name) <= TId(parentName))
//        case _ => None
//      }
//
//      // Dependency on 'MODULES' for the 'm' state of tristate configs
//      val modConstraint: Option[BExpr] =
//        if (isModulesConfigDefined && t == KTriType)
//          Some((BId(name) & !BId(name + "_m")) implies BId(MODULES_CONFIG_NAME))
//        else None
//
//      // Disallow the 'y' state if the config has a dependency on '&& m'
//      val modOnlyConstraint: Option[BExpr] =
//        if (modOnly) Some(!BId(name + "_m"))
//        else None
//
//      (rds map { case (e, id) => id beq e }) ::: // reverse dependency sub-expressions
//        rdsEquiv :: // reverse dependency equivalence
//        (rdsId <= TId(name)) ::  // reverse dependency lower bound
//        typeConstraint.toList :::
//        // upperBoundConstraints.toList ::: FIXME ignoring upper bound
//        tristateConstraints :::
//        undefinedConstraints :::
//        modConstraint.toList :::
//        modOnlyConstraint.toList :::
//        defaults(defs) // defaults
//  }
//
//}
