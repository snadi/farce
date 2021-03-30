package gsd.farce.comparisons

import gsd.linux._
import gsd.farce.features.model.{CDLExprUtils, BExprUtils}
import de.fosd.typechef.featureexpr.sat.{SATFeatureExprFactory, SATFeatureModel}
import gsd.farce.utilities.PropertyKeys._
import gsd.linux.Or
import gsd.linux.And
import gsd.linux.Id
import gsd.linux.BId
import gsd.linux.Not
import gsd.linux.BNot
import gsd.linux.BImplies
import gsd.linux.Eq
import gsd.linux.BOr
import gsd.linux.BAnd
import gsd.linux.NEq

import org.kiama.rewriting.Rewriter._
import gsd.linux.BId
import gsd.linux.BImplies
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import scala.collection.parallel.ParSet
import scala.collection.Set
import gsd.cdl.parser.EcosIml
import gsd.cdl.model.{CDLExpression, Identifier}
import java.io.{FileWriter, PrintWriter}

/**
 * Created by berger on 20/02/14.
 */
object NaiveCTCExtractorMain {

  case class Model( name: String, modelFile: String, dimacs: String )

  val models = Model( "busybox-1.21.0", "../variability-models/kconfig/busybox-1.21.0.exconfig", "../variability-models/kconfig/abstraction/busybox-1.21.0.dimacs" ) ::
               Model( "uClibc-0.9.33.2", "../variability-models/kconfig/uClibc-0.9.33.2.exconfig", "../variability-models/kconfig/abstraction/uClibc-0.9.33.2.dimacs" ) ::
               Model( "linux-2.6.33.3", "../variability-models/kconfig/linux-2.6.33.3.exconfig", "../variability-models/kconfig/abstraction/linux-2.6.33.3.dimacs" ) ::
               Model( "eCos-3.0(pc_vmWare)", "../variability-models/cdl/pc_vmWare.iml", "../variability-models/cdl/abstraction/pc_vmWare.dimacs" ) :: Nil


  def main( args:Array[String]){

    for( m <- models ){

      val isKconfig = m.modelFile.endsWith( ".exconfig") || m.modelFile.endsWith(".pb")
      val isCDL = m.modelFile.endsWith( ".iml")

      val tcImpls = if( isKconfig )
        getKconfigCTConstraints( m )
      else if( isCDL )
        getCDLCTConstraints( m )
      else
        sys.error( "Unknown file type: " + m.modelFile )

      val fm = SATFeatureModel.createFromDimacsFile( m.dimacs, "", "" )

      val holding = tcImpls.filter( _._1 isTautology fm )

      println( "Model: " + m.name + ", unique expressions: " + tcImpls.size +
        ", holding in model: " + holding.size +
        " (" + ( ( holding.size.toFloat / tcImpls.size.toFloat ) * 100 ).toInt + "%)" )

      val out = new PrintWriter( new FileWriter( "output/" + m.name + "-naiveCTCs.txt" ))

      holding.seq foreach{ e => { e._1 print out; out println } }

      out close

      println( "Patterns:" )
      if( isCDL )
        findCDLPatterns( holding.map( _._2.asInstanceOf[CDLExpression] ).seq )
      if( isKconfig )
        findKconfigPatterns( holding.map( _._2.asInstanceOf[BExpr] ).seq )
      println
    }

//    val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
//    val test = featureExprParser.parse( "CONFIG_ASH_ALIAS => CONFIG_ASH" )
//    // dead features
//    linuxK.allConfigs foreach{ c =>
//      if( FeatureExprFactory.createDefinedExternal( "CONFIG_" + c.name ).not.isTautology( linuxFM ) )
//        println( c.name + "is dead" )
//    }
  }

  def findCDLPatterns( expressions: Set[CDLExpression] ){

    val uniExps = for( e <- expressions.toList ) yield {

      val ids = collectl{
        case Identifier( i ) => i
      }(e)
      val idMap = ids.zipWithIndex.map{case (a,b) => (a, (b+97).toChar )}.toMap

      val unifyIDs = everywherebu{
        rule{
          case Identifier( n ) => Identifier( idMap.get(n).get.toString )
        }
      }

      rewrite( unifyIDs )( e )
    }

    val grouped = uniExps.groupBy( x => x ).toList.sortBy( _._2.size ).reverse
    grouped foreach { g =>
      println( g._1 + ", count: " + g._2.size )
    }

  }

  def findKconfigPatterns( expressions: Set[BExpr] ){

    val uniExps = for( e <- expressions.toList ) yield {

      val ids = collectl{
        case BId( i ) => i
      }(e)
      val idMap = ids.zipWithIndex.map{case (a,b) => (a, (b+97).toChar )}.toMap

      val unifyIDs = everywherebu{
        rule{
          case BId( n ) => BId( idMap.get(n).get.toString )
        }
      }

      rewrite( unifyIDs )( e )
    }

    val grouped = uniExps.groupBy( x => x ).toList.sortBy( _._2.size ).reverse
    grouped foreach { g =>
      println( g._1 + ", count: " + g._2.size )
    }
  }

  private val fixCaseRule = everywherebu{
    rule{
      case BId( n ) => BId( "CONFIG_" + n )
    }
  }

  private def getKconfigCTConstraints(m: NaiveCTCExtractorMain.Model): ParSet[Tuple2[FeatureExpr,BExpr]] = {
    val k = KConfigParser.parseKConfigFile( m.modelFile )

    val impls = //rewrite(fixCaseRule)(
      k.allConfigs.flatMap(c =>
      c.depends.map(d => BImplies(BId(c.name), conv(d.cond))) :::
        c.sels.map(s => BImplies(BId(c.name), BId(s.id)))
    ).toSet //)

    impls.map( e => ( BExprUtils.toTypeChefExpr(e), e.asInstanceOf[BExpr] ) ).par
  }

  private def getCDLCTConstraints( m: Model ): ParSet[Tuple2[FeatureExpr,CDLExpression]] = {
    import CDLExprUtils._
    val iml = EcosIml.CupParser.parseFile( m.modelFile )
    val impls = iml.allNodes.flatMap( n =>
      n.activeIfs.filter( isBooleanExpression ).map( ai => Identifier( n.id ) implies ai ) :::
      n.reqs.filter( isBooleanExpression ).map( ai => Identifier( n.id ) implies ai )
    ).toSet

    impls.map( e => ( toTypeChefExpr(e), e ) ).par
  }

  def conv( kexpr: KExpr ): BExpr = kexpr match{
    case And( l, r ) => BAnd( conv(l), conv(r) )
    case Or( l, r ) => BOr( conv(l), conv(r) )
    case Eq( l, r ) => BTrue
    case NEq( l, r ) => BTrue
    case Not( e ) => BNot( conv(e) )
    case Id( n ) => BId( n )
    case Mod => BTrue
    case e => sys error ("not supported: " + e )
  }

}
