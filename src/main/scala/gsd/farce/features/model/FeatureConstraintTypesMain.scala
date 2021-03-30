package gsd.farce.features.model

import java.io.{FilenameFilter, File, PrintStream}
import gsd.linux._
import gsd.linux.stats.ASEStatistics
import gsd.linux.BId
import scala.Some
import gsd.linux.TId
import gsd.linux.AConfig
import gsd.linux.ADefault
import gsd.linux.AChoice
import org.kiama.rewriting.Rewriter._
import gsd.linux.cnf._
import scala.util.logging.ConsoleLogger

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 13.05.13
 * Time: 18:36
 * To change this template use File | Settings | File Templates.
 */
object FeatureConstraintTypesMain {

  // for REPL
  lazy val uclibcCKconfig = KConfigParser.parseKConfigFile( "../variability-models/kconfig/uClibc-0.9.33.2.exconfig" )

  def main( args: Array[String] ){

      args match {
        case Array(i,o) => exportConstraintTypes(i, new PrintStream( o ) )
        case Array(i) => exportConstraintTypes( i )
        case _ => sys.error( "FeatureConstraintTypesMain <extract file> [output file]")
      }

    }

  def exportConstraintTypes( inFile: String, out: PrintStream = System.out ){
    if( inFile.endsWith( "exconfig" ) )
      exportKconfigConstraintTypes( inFile, out )
    else List[FExpr]()
  }

  def batchConvertExconfigs( inFolder: String, outFolder: String = "" ){
    val _outFolder = if( outFolder == "" ) inFolder else outFolder
    val exconfigs = new File( inFolder ).listFiles( new FilenameFilter {
      def accept(dir: File, name: String) = name.endsWith(".pb")
    })
    for ( exconfig <- exconfigs ){
      println( exconfig.getPath )
      exportKconfigFullConstraints( exconfig.getPath, new PrintStream( _outFolder + "/" +  exconfig.getName.substring( 0, exconfig.getName.size - 3) + ".dimacs" ) )
    }
  }

  def test{
//    val f = "../variability-models/kconfig/abstraction/uclibc.dimacs"
//    val f = "../variability-models/kconfig/abstraction/uclibc_tristate.dimacs"
    val f = "uClibc-0.9.33.2__crosstree.dimacs"
//    val f = "uClibc-0.9.33.2__full.dimacs"
    val dh = DimacsReader.readHeaderFile( f )
    val dp = DimacsReader.readFile( f )

    val sat = new SATBuilder(dp.cnf, dp.numVars, dh.generated, dh.firstGen)
                    with ImplBuilder with ConsoleLogger

    println( sat.implication( dh.idMap.get("UCLIBC_HAS_LOCALE").get, dh.idMap.get("UCLIBC_HAS_WCHAR").get ))

//    println( sat.isVarsSat( 440 ) )
//
////    WCHAR 439  UCLIBC_HAS_LOCALE 207
//    println( sat.implication( 439, 207 ) )
//    println( sat.implication( 207, 439 ) )
////    c 515 UCLIBC_HAS_XLOCALE
//
//    println( sat.implication( 515, 207 ))
//    println( sat.implication( 515, 439 ))

  }

  /**
   * without hierarchy and without groups
   * @param inFile
   * @param out
   */
  def exportKconfigCrossTreeConstraints( inFile: String, out: PrintStream ) {
      val k = KConfigParser.parseKConfigFile( inFile )

      val k_without_hierarchy = ASEStatistics.removeInherited( k )
      val trans_without_hierarchy = new TristateTranslation( k_without_hierarchy.toAbstractKConfig, true, true, false )

      val exp_woh = trans_without_hierarchy.translate.toSet

      val cnf = exp_woh flatMap { e =>
        e.toCNF( trans_without_hierarchy.idMap)
      }

      val varMap: Map[Int, String] = (trans_without_hierarchy.idMap map { case (id,v) => (v, id)}).toMap

      out.println( cnf.toDimacs(varMap, trans_without_hierarchy.generated.toSet map trans_without_hierarchy.idMap.apply))

      out close

  }

  /**
   * including hierarchy and groups; allows to partially pre-configure the model
   * @param inFile .exconfig file
   * @param out output stream
   * @param ffs list of features to set to false
   * @param tfs list of features to set to true
   */
  def exportKconfigFullConstraints( inFile: String, out: PrintStream, tfs: Set[String] = Set.empty, ffs: Set[String] = Set.empty ) {
    val k = KConfigParser.parseKConfigFile( inFile )

    val (cnf,generated,idMap) = kconfig2CNFfull(k, tfs, ffs)

    val varMap: Map[Int, String] = (idMap map { case (id,v) => (v, id)}).toMap

    out.println( cnf.toDimacs(varMap, generated map idMap.apply))

    out close

  }


  def kconfig2CNFfull(k: ConcreteKConfig, tfs: Set[String], ffs: Set[String]) = {
    val trans = new TristateTranslation(k.toAbstractKConfig, true, true, true)

    val partialConfigRule = everywherebu {
      rule {
        case BId(id) if tfs contains id => BTrue
        case BId(id) if ffs contains id => BFalse
      }
    }

    val exp_wh = rewrite(partialConfigRule)(trans.translate.toSet)

    val idMap = trans.idMap.--(tfs).--(ffs)

    val cnf = exp_wh flatMap {
      e =>
        e.toCNF(idMap)
    }
    (cnf,trans.generated.toSet,idMap)
  }

  def exportKconfigGroupConstraints( inFile: String, out: PrintStream ) {
    val k = ASEStatistics.removeInherited( KConfigParser.parseKConfigFile( inFile ) )
    val trans = new TristateTranslation( k.toAbstractKConfig, true, false, true )
    val exp_wh = trans.translate.toSet
    val cnf = exp_wh flatMap { e =>
      e.toCNF( trans.idMap)
    }

    val varMap: Map[Int, String] = (trans.idMap map { case (id,v) => (v, id)}).toMap

    out.println( cnf.toDimacs(varMap, trans.generated.toSet map trans.idMap.apply))

    out close

  }

  def exportKconfigConstraintTypes( inFile: String, out: PrintStream ) {
    val k = KConfigParser.parseKConfigFile( inFile )

    val features = k.allConfigs
    val s = new ASEStatistics( k )
    val trans = new TristateTranslation( k.toAbstractKConfig )

    println( "features: " + features.size )

    println( "with hierarchy:")
    val exp_wh = trans.translate.toSet
    println( trans.translate.size)
    println( exp_wh.size)
//    trans.translate foreach println

    val k_without_hierarchy = ASEStatistics.removeInherited( k )
    val trans_without_hierarchy = new TristateTranslation( k_without_hierarchy.toAbstractKConfig )

    println( "without hierarchy:")
    val exp_woh = trans_without_hierarchy.translate.toSet
    println( trans_without_hierarchy.translate.size)
    println( exp_woh.size)
//    trans_without_hierarchy.translate foreach println

    println( "with hierarchy -- without hierarchy")
    println( (exp_wh -- exp_woh).size )
    (exp_wh -- exp_woh) foreach println

    val hierarchyExps = exp_wh -- exp_woh
    val idsHierarchyExps = collects{
      case BId( i ) => i
    }(hierarchyExps)
    val featuresInHierarchyExps = idsHierarchyExps.filterNot( _ startsWith "_X" ).filterNot( _ endsWith "_m" )
    println( "IDs in hierarchy expressions: " + featuresInHierarchyExps.size )
    println( "features missing in hierarchy expressions: " )
    ( k.allConfigs.map(_.name).toSet -- featuresInHierarchyExps ) foreach println

    val inters = exp_wh.intersect( exp_woh )
    println( "intersection: " + inters.size)



    val idsInExpWH = collects{
      case BId( i ) => i
    }(exp_wh)
    println( "IDs in exp with hierarchy: " + idsInExpWH.size )

    val idsInExpWoH = collects{
      case BId( i ) => i
    }(exp_woh)
    println( "IDs in exp without hierarchy: " + idsInExpWoH.size )



//    //////////////////////////////////
//    ASEStatistics.removeInherited( k ).toAbstractKConfig.configs.map{ case AConfig(nodeId, name, t, inh, pro, defs, rev, ranges, modOnly) =>
//      defs
//    }



    val cnf = exp_woh flatMap { e =>
      e.toCNF( trans_without_hierarchy.idMap)
    }

    val varMap: Map[Int, String] = (trans_without_hierarchy.idMap map { case (id,v) => (v, id)}).toMap

    out.println( cnf.toDimacs(varMap, trans_without_hierarchy.generated.toSet map trans_without_hierarchy.idMap.apply))

//
//    lazy val id2int = ( ( features ++ xorTransform.generated.toSet ).
//                            zipWithIndex map { case ( a, b ) => (a, b + 1 ) } ).toMap
//        lazy val int2id = ( id2int map { case ( a, b ) => ( b, a ) } ).toMap
//
//        val cnf = ( xorTransform.expressions ::: simpleConjunction ).flatMap( _.toCNF( id2int ) )
//
//    trans_without_hierarchy.translate.
//        out println cnf.toDimacs( int2id, xorTransform.generated.toSet.map( id2int ) )

    out close


  }



}

class TristateTranslation(val k: AbstractKConfig, val addUndefined: Boolean = true, val includingConfigConstraints: Boolean = true, val includingGroups: Boolean = true ) {

  val MODULES_CONFIG_NAME = "MODULES"

  val isModulesConfigDefined = k.findConfig(MODULES_CONFIG_NAME).isDefined

  object IdGen {
    val prefix = "_X"
    var i = 0
    def next = { i+=1; prefix + i }
    def allIds = (1 to i).map { prefix + _ }.toList
  }

  def generated: List[String] =
    IdGen.allIds ::: (IdGen.allIds map { _ + "_m"})

  def identifiers: List[String] =
    k.identifiers.toList ::: IdGen.allIds

  def size: Int = identifiers.size

  /**
   * Var i (odd) represents identifier x, Var i+1 (even) represents x_m
   */
  def idMap: Map[String, Int] =
    Map() ++ {
      (identifiers flatMap
              { id => List(id, id + "_m") }).zipWithIndex map
                     { case (id,i) => (id, i + 1) }
    }

  val literalMap: Map[(String, KExpr), String] =
    (k.literalExprs map (_ -> IdGen.next)).toMap

  /**
   * function instantiated with a map from string literal to generated variable
   */
  val toTExpr: KExpr => TExpr = TExpr.toTExpr(literalMap)

  /**
   * Stateful: Changes identifiers in IdGen
   */
  lazy val translate: List[BExpr] ={
    var ret:List[BExpr] = Nil
    if( includingConfigConstraints )
      ret = ( k.configs flatMap translate )
    if( includingGroups )
      ret = ret ::: (k.choices flatMap translate)

    ret map (_.simplify) filter (_ != BTrue)
  }
//    if( includingGroups )
//      ((k.configs flatMap translate) ::: (k.choices flatMap translate)) map
//        (_.simplify) filter (_ != BTrue)
//    else
//      (k.configs flatMap translate) map
//              (_.simplify) filter (_ != BTrue)


  def translate(c: AChoice): List[BExpr] = c match {
    // Boolean, mandatory choice == XOR group
    case AChoice(vis, true, isOpt, members) =>
      // exclusions between all members
      val exclusions = Combinations.choose(2, members) map {
        case List(x,y) => !BId(x) | !BId(y)
        case _ => sys.error("This should never occur since we use Combinations.choose(2,...)")
      }

     val mandImpl =
       if (isOpt) Some((toTExpr(vis) > TNo) implies members.map(BId(_): BExpr).reduce(_|_))
       else None

     mandImpl.toList ::: exclusions

    // any other kind of group doesn't impose a constraint
    case _ => Nil
  }

  /**
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {

    // TODO verify that environment variables shouldn't impose any constraint
    case AConfig(_,name, _, _, _, _, _, _, _) if k.env contains name =>
      Nil

    case AConfig(nodeId, name, t, inh, pro, defs, rev, ranges, modOnly) =>

      // use a generated variable for each expression in the reverse dependency
      val rds = rev map { r => (toTExpr(r), TId(IdGen.next)) }

      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }

      // rdsEquiv will be Some if a generated variable is used to represent
      // entire reverse dependency expression
      val (rdsId, rdsEquiv): (TId, BExpr) = {
          val id = IdGen.next
          (TId(id), rdsExpr beq TId(id))
        }

      // create default constraints
      def defaults(rest: List[ADefault]): List[BExpr] = {

        // uses a BId to represent the negated previous conditions
        def traverse(rest: List[ADefault], prevCondId: BId): List[BExpr] = rest match {
          case Nil => Nil

            // FIXME not using prev on ADefault
          case ADefault(e,_,cond)::tail =>

            // generate condition id for next iteration of t(..)
            //TODO optimization: not necessary if tail is empty
            val (nextCondId, nextCondEquiv) = {
              val id = IdGen.next
              val (bid1, bid2) = (BId(id), BId(id + "_m"))

              //FIXME id generator still generates _2 variable, so we make it dead
              (bid1, (bid1 iff (prevCondId & (toTExpr(cond) beq TNo))) & !bid2)
            }

            // antecedent for current default
            val ante = prevCondId & (toTExpr(cond) > TNo)

            //
            // Handle defaults differently for tristate and non-tristate configs
            //
            val defaultExpr = t match {
              case KStringType | KIntType | KHexType =>
                // FIXME check if an entry in the map exists
                ante implies BId(literalMap((name, e)))

              case  _ =>
                // Handle default y quirk (i.e. if default y, then config takes
                // value of its condition, not y)
                val tex = if (e == Yes) toTExpr(cond) else toTExpr(e)

                // tex | rdsId: config takes the max of the default or the RDs
                ante implies (TId(name) beq (tex | rdsId))
            }
            defaultExpr ::
              nextCondEquiv :: // default condition equivalence
              traverse(tail, nextCondId)
        }

        // negated prompt condition for defaults (since prevCondId represents
        // the negated previous condition)
        val (proId, proEquiv) = {
          val id = IdGen.next
          val (bid1, bid2) = (BId(id), BId(id + "_m"))

           //FIXME id generator still generates _2 variable, so we make it dead
          (bid1, (bid1 iff (toTExpr(pro) beq TNo)) & !bid2)
        }

        // The prompt acts as the first negated condition
        proEquiv :: traverse(rest, proId)
      }

      // Disallow mod state from Boolean, String, Int and Hex configs
      val typeConstraint: Option[BExpr] = t match {
        case KBoolType | KStringType | KIntType | KHexType =>
          Some(BId(name) implies BId(name + "_m") )
        case _ => None
      }

      // Disallow (0,1) state
      val tristateConstraints: List[BExpr] =
        AbstractKConfig.identifiers(c).toList map { id => BId(id) | !BId(id + "_m") }

      // Make undefined variables dead referenced in this config
      val undefinedConstraints: List[BExpr] =
        if (addUndefined) ((AbstractKConfig.identifiers(c) -- (k.configs map (_.name)) -- k.env) map (!BId(_))).toList
        else Nil

      // an upper bound is only imposed on tristate configs, if it's parent is also tristate
      // FIXME not used
      val upperBoundConstraints: Option[BExpr] = (t, k.parentMap.get(c)) match {
        case (KTriType, Some(AConfig(_,parentName,KTriType,_,_,_,_,_,_))) => Some(TId(name) <= TId(parentName))
        case _ => None
      }

      // Dependency on 'MODULES' for the 'm' state of tristate configs
      val modConstraint: Option[BExpr] =
        if (isModulesConfigDefined && t == KTriType)
          Some((BId(name) & !BId(name + "_m")) implies BId(MODULES_CONFIG_NAME))
        else None

      // Disallow the 'y' state if the config has a dependency on '&& m'
      val modOnlyConstraint: Option[BExpr] =
        if (modOnly) Some(!BId(name + "_m"))
        else None

      (rds map { case (e, id) => id beq e }) ::: // reverse dependency sub-expressions
        rdsEquiv :: // reverse dependency equivalence
        (rdsId <= TId(name)) ::  // reverse dependency lower bound
        typeConstraint.toList :::
        // upperBoundConstraints.toList ::: FIXME ignoring upper bound
        tristateConstraints :::
        undefinedConstraints :::
        modConstraint.toList :::
        modOnlyConstraint.toList :::
        defaults(defs) // defaults
  }

}