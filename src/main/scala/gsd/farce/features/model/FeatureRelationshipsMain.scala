package gsd.farce.features.model

import gsd.linux._
import gsd.linux.Hierarchy._
import java.io._
import org.kiama.rewriting.Rewriter._
import gsd.cdl.tse11.TSE11Statistics
import gsd.cdl.model.{Identifier, Parent}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.util.Properties
import gsd.linux.Prompt
import gsd.linux.Id
import gsd.linux.CMenu
import scala.Some
import gsd.linux.CConfig
import gsd.linux.ConcreteKConfig
import gsd.linux.CChoice
import gsd.linux.stats.ASEStatistics
import gsd.farce.utilities.Utilities._
import gsd.commons.{Logging, MyArgot}
import gsd.farce.FarceMain
import gsd.farce.utilities.{Config, Utilities}
import org.clapper.argot.{ArgotConverters, ArgotUsageException}
import gsd.cdl.parser.EcosIml
import gsd.farce.features.model.FeatureRelationships.{Edge, EdgeType}
import gsd.farce.features.model.FeatureRelationships.EdgeType.EdgeType
import scala.io.Source
import gsd.farce.utilities.PropertyKeys._


/**
* Created with IntelliJ IDEA.
* User: berger
* Date: 19.04.13
* Time: 12:26
* To change this template use File | Settings | File Templates.
*/
object FeatureRelationshipsMain extends MyArgot with Logging{

  val name = "FeatureRelationshipsMain"
  val toolVersion = "FeatureRelationshipsMain " + FarceMain.globalVersionString

  import ArgotConverters._
  import EdgeType._

  val system = parser.parameter[String]("system", "name of the system to analyze (" + Utilities.getSupportedSystemsAsString + ")", false)
  val modelFile = parser.parameter[String]("model", "filename of the corresponding variability model", false)
  // TODO: FIXME - prefix and suffix should be options
  val prefix = parser.parameter[String]("prefix", "FIXME: description", true )
  val suffix = parser.parameter[String]("suffix", "FIXME: description", true )

  val properties = new Properties()
  var defineMap = Map[String,String]()
  var noDefineList = List[String]()

  def main(args: Array[String]) {

    try{
      parser.parse( args )
      loadPropertiesFile( system.value.get, properties)

      val pre = prefix.value.getOrElse( "" )
      val suf = suffix.value.getOrElse( "" )

      // FIXME: not sure why feature model is always taken from properties
      val featureModel = SATFeatureModel.createFromDimacsFile(properties.getProperty(FEATURE_MODEL_FILE), pre, suf )

      val relFinder = FeatureRelationships.getFeatureRelationshipFinder( modelFile.value.get, pre, suf )

      val nonBooleanFeatures = relFinder.nonBooleanFeatures

      defineMap = relFinder.definesMap
      noDefineList = relFinder.noDefineList

      val edges = relFinder.getEdges.filterNot( _.b == "root" )
      printEdges( edges.filter( _.t == hierarchy ), properties.getProperty(HIERARCHY_CONSTRAINTS_FILE), featureModel, nonBooleanFeatures, pre, suf )
      printEdges( edges.filter( _.t == crosstree), properties.getProperty(CROSSTREE_CONSTRAINTS_FILE), featureModel, nonBooleanFeatures, pre, suf )

    }catch{
      case e:ArgotUsageException => println( e.getMessage )
    }
  }


  private def getDisplayName(id: String, pre: String, suf: String) = pre + getDefineValue(id) + suf

  private def printEdges( edges: List[Edge], outputFile: String, featureModel: SATFeatureModel, nonBooleanFeatures: Set[String], pre: String, suf: String ){
    val writer = new PrintWriter(new FileWriter( outputFile ))
    ldebug( "have " + edges.size + " edges" )
    var count = 1

    for ( edge <- edges ){
      // System.err.println("analyzing edge " + count)
      //only deal with boolean features. Ignore edge if either features are non-boolean
      val firstFeatureExpr = FeatureExprFactory.createDefinedExternal(pre + edge.a + suf)
      val secondFeatureExpr =  FeatureExprFactory.createDefinedExternal(pre + edge.b + suf)

      if (!nonBooleanFeatures.contains(edge.a) && !nonBooleanFeatures.contains(edge.b)) {
        if ((firstFeatureExpr.equiv(secondFeatureExpr)).isTautology(featureModel)) {
          writeRelationship(edge.a, "<=>", edge.b, pre, suf, writer)
        } else if ((firstFeatureExpr.implies(secondFeatureExpr)).isTautology(featureModel) ) {
          writeRelationship(edge.a, "=>", edge.b, pre, suf, writer)
        } else if ((firstFeatureExpr.implies(secondFeatureExpr.not())).isTautology(featureModel)) {
          writeRelationship(edge.a, "=> !", edge.b, pre, suf, writer, true)
        } else if ((secondFeatureExpr.implies(firstFeatureExpr)).isTautology(featureModel)) {
          writeRelationship(edge.b, "=> ", edge.a, pre, suf, writer)
        } else if ((secondFeatureExpr.implies(firstFeatureExpr.not())).isTautology(featureModel)) {
          writeRelationship(edge.b, "=> !", edge.a, pre, suf, writer, true)
        } else
          System.err.println( "NO relationship found for " + edge.t + " edge: " + pre + edge.a + suf + " , " + pre + edge.b + suf )
        count += 1
      }
    }

    writer close
  }

  def writeRelationship(n1:String, reln:String, n2:String,  pre: String, suf:String, writer: PrintWriter, putBracket: Boolean = false){
    if(!noDefineList.contains(pre + n1 + suf) && !noDefineList.contains(pre +  n2 + suf))
      if(putBracket)
        writer.println(getDisplayName(n1, pre, suf) + " " + reln +" (" + getDisplayName(n2, pre, suf) + ")")
      else
        writer.println(getDisplayName(n1, pre, suf) + " " + reln +" " + getDisplayName(n2, pre, suf))
    else{
      System.err.println("NO DEFINE FOUND FOR: " + pre + n1 + suf + " " + reln +" " +  pre + n2 + suf)
    }
  }

  def getDefineValue(id: String): String = {
    defineMap.get(id) match {
      case Some(p) => p
      case None => id
    }
  }

}

object FeatureRelationships{

  case class Edge(a: String, b: String, t: EdgeType)

  object EdgeType extends Enumeration {
    type EdgeType = Value
    val hierarchy, crosstree = Value
  }

  def getFeatureRelationshipFinder( modelFile: String, pre: String = "", suf: String = "" ) = {
    if( modelFile endsWith "iml" )
      new CDLFeatureRelationshipFinder( modelFile, pre, suf )
    else if( modelFile endsWith "exconfig" )
      new KconfigFeatureRelationshipFinder( modelFile, pre, suf )
    else if( modelFile endsWith "pb" )
          new KconfigFeatureRelationshipFinder( modelFile, pre, suf )
    else
      sys error "unknown file type: " + modelFile

  }

}

trait FeatureRelationshipFinder extends Logging{

  val prefix, suffix: String
  def getEdges(): List[Edge]

  val nonBooleanFeatures: Set[String]

  //mainly used for CDL where another macro may be used as a defn
  val definesMap: Map[String,String]

  //mainly for CDL so that we don't display options that are no define
  val noDefineList: List[String]
}

class CDLFeatureRelationshipFinder( inFile: String, val prefix: String = "", val suffix: String = "" ) extends FeatureRelationshipFinder{

  import EdgeType._

  val iml = EcosIml.CupParser.parseFile( inFile )
  val es = new TSE11Statistics( iml )

  val nonBooleanFeatures = FeatureTypesMain.getCDLDataFeatures( iml )

  val definesMap = es.model.defines

  val noDefineList = es.features.filter(p => p.no_define).map(_.id)

  def getEdges(): List[Edge] = {

    val es = TSE11Statistics(inFile)


    val hEdges: List[Edge] = es.features.map {
      f =>
        es.model.childParentMap.get(f.id) match {
          case Some(p) => new Edge(f.id, p, hierarchy)
          case None => new Edge(f.id, "root", hierarchy)
        }
    }

    val cEdges: List[Edge] = es.features.flatMap {
      f =>
        es.referencedIDsPerFeature.get(f.id).get.map {
          refFeature =>
            Edge(f.id, refFeature, crosstree)
        }
    }.filterNot( c => hEdges.contains( Edge( c.a, c.b, hierarchy ) ) )

    /*  out.println( "origin,target(parent),type" )
    (hEdges ++ cEdges) foreach { e =>
      out.println( e.a + "," + e.b + "," + e.t )
    }

    out.close*/

    hEdges ++ cEdges
  }

}

class KconfigFeatureRelationshipFinder( modelFile: String, val prefix: String = "", val suffix: String = "" ) extends FeatureRelationshipFinder{

  import EdgeType._

  case class KconfigFeature(name: String) {
    override def toString = name
  }

  val kconfig = KConfigParser.parseKConfigFile( modelFile )
  val nonBooleanFeatures = FeatureTypesMain.getKconfigDataFeatures( kconfig )

  //return empty map
  val definesMap = Map[String,String]()
  val noDefineList = Source.fromFile(new File("notusedfeatures")).getLines().toList //TODO: do not hard code it

  def getEdges(): List[Edge] = {

    val features = kconfig.allConfigs
    val hEdges = hierarchyEdges(mkHierarchyMap(kconfig), features, "root")
    val cEdges = crossTreeEdges( ASEStatistics.removeInheritedAndDependsOn( kconfig )).filterNot( c => hEdges.contains( Edge( c.a, c.b, hierarchy ) ) )

    //    val features = ( hEdges.map( _.a ) ::: hEdges.map( _.b ) ) toSet

//    def clean(s: String) = s.replace("Linux Kernel Configuration", "root")

    // out.println( "origin,target(parent),type" )
    //(hEdges ++ cEdges) foreach { e =>
    //  out.println( clean( e.a ) + "," + clean( e.b ) + "," + e.t )
    ///}

    //out.close

    hEdges ++ cEdges
  }


  private def hierarchyEdges(hmap: Map[CSymbol, CSymbol], features: Iterable[CSymbol], root: String) =
    features map {
      f =>
        f -> getParentNonAbstractFeature(f, hmap)
    } flatMap {
      case (x, Some(y)) => Edge(getName(x), getName(y), hierarchy) :: Nil
      case (x, None) => Edge(getName(x), root, hierarchy) :: Nil
    } toList

  private def getParentNonAbstractFeature(f: CSymbol, hmap: Map[CSymbol, CSymbol]): Option[CConfig] =
    hmap.get(f) match {
      case Some(c: CConfig) => Some(c)
      case Some(s: CSymbol) => getParentNonAbstractFeature(s, hmap)
      case None => None

    }

  def crossTreeEdges(k: ConcreteKConfig): Set[Edge] = {

    lazy val nodeIdsToIds: Map[Int, Set[String]] = mkNodeIdToReferencedIds(k.features)
    lazy val nodeIdToSymbol = k.features.map(c => (c.nodeId, c)).toMap

    var configsToConfigs = Map[CConfig, Set[CConfig]]()

    nodeIdsToIds.foreach {
      case (n, set) =>
        nodeIdToSymbol.get(n) match {
          case Some(c: CConfig) => configsToConfigs += (c -> set.flatMap {
            i =>
              k.configMap.get(i) match {
                case Some(cc) => Set[CConfig](cc)
                case None => Set.empty[CConfig]
              }
          })
          case _ =>;
        }
    }


    configsToConfigs.flatMap {
      case (c, set) =>
        set.map(s => new Edge(c.name, s.name, crosstree))
    } toSet


  }

  def getName(c: CSymbol) = c match {
    case CConfig(_, name, _, _, _, _, _, _, _, _, _, _) => name
    case CMenu(_, p, _) => p text
    case CChoice(_, p, _, _, _, _, _) => p text
    case _ => "UNKNOWN"
  }


  def getNameForAnyFeature(f: CSymbol): String = f match {
    case c: CConfig => c.name
    case c: CMenu => c.prompt.text
    case c: CChoice => "" + c.nId
    case c: CSymbol => "" + c.nodeId
  }

  def getNameForAnyFeature(f: Option[CSymbol]): String = f match {
    case Some(s) => getNameForAnyFeature(s)
    case None => "none"
  }

  private def mkNodeIdToReferencedIds(in: List[CSymbol]): Map[Int, Set[String]] = {
    in map {
      case c: CConfig =>
        (c.nodeId, collects {
          case Id(s) => s
        }(c.properties ::: c.depends) ++ (c.sels map {
          _.id
        }))
      case c: CChoice =>
        (c.nodeId, collects {
          case Id(s) => s
        }(c.properties))
      case CMenu(nId, Prompt(_, e), _) =>
        (nId, collects {
          case Id(s) => s
        }(e))
      case _ =>
        sys.error("If conditions shouldn't appear here!")
    }
  }.toMap


}
