package gsd.farce.features.model

import gsd.linux._
import gsd.linux.stats.FeatureStatistics
import gsd.cdl.tse11.TSE11Statistics
import gsd.farce.utilities.PropertyKeys._
import gsd.linux.CChoice
import gsd.linux.CConfig
import java.util.Properties
import de.fosd.typechef.featureexpr.{FeatureExprParser, FeatureExprFactory}
import java.io.{File, FileWriter, PrintWriter}
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 01.05.13
 * Time: 15:27
 * To change this template use File | Settings | File Templates.
 */
object FeatureGroupsMain {

  var prefix, suffix = ""
  val properties = new Properties()
  var nonBooleanFeatures = Set[String]()

  object GroupType extends Enumeration {
    type GroupType = Value
    val gXOR, gMUTEX, gOR = Value
  }

  import GroupType._

  case class FeatureGroup(parent: String, groupedFeatures: List[String], gtype: GroupType)

  def main(args: Array[String]) {

    args match {
      case Array(s, i) => {
        loadPropertiesFile(s, properties)
        exportGroups(i)
      }
      case Array(s, i, pre) => {
        loadPropertiesFile(s, properties)
        prefix = pre
        exportGroups(i)
      }
      case Array(s, i, pre, suff) => {
        loadPropertiesFile(s, properties)
        prefix = pre
        suffix = suff
        exportGroups(i)
      }
      case _ => sys.error("Usage: FeaturesGroupMain <system name> <input file> [prefix] [suffix]")
    }

  }

  def exportGroups(inFile: String): List[FeatureGroup] = {
    inFile.split('.').toList match {
      case f :: "iml" :: Nil => {
        nonBooleanFeatures = FeatureTypesMain.getDataFeatures(inFile)
        getCDLGroups(inFile, prefix, suffix, true)
      }
      case f :: "exconfig" :: Nil => {
        nonBooleanFeatures = FeatureTypesMain.getKconfigDataFeatures(inFile)
        getKconfigGroups(inFile, prefix, suffix, true)
      }
      case _ => List[FeatureGroup]()
    }

  }

  def containsNonBooleanFeature(featuresToCheck: List[String]): Boolean = {

    for (feature <- featuresToCheck) {
      if (nonBooleanFeatures.contains(feature)) {
        false
      }
    }
    true
  }

  def printGroups(groups: List[FeatureGroup], outputFile: String) {
    val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
    val writer = new PrintWriter(new FileWriter(new File(outputFile)))

    for (group <- groups) {
      var constraint = ""
      if (!containsNonBooleanFeature(group.groupedFeatures)) {
        if (group.gtype == GroupType.gXOR) {
          constraint = featureExprParser.oneOf(group.groupedFeatures.map(featureExprParser.parse(_))).toString()
        } else if (group.gtype == GroupType.gOR) {
          constraint = featureExprParser.atLeastOne(group.groupedFeatures.map(featureExprParser.parse(_))).toString()
        } else if (group.gtype == GroupType.gMUTEX) {
          constraint = featureExprParser.atMostOne(group.groupedFeatures.map(featureExprParser.parse(_))).toString()
        }

        writer.println(constraint)
      } else {
        System.err.println("group: " + group + " contains non-boolean feature")
      }
    }
    writer.close()
  }

  def getKconfigGroups(extractFile: String, prefix: String, suffix: String, print: Boolean = false) = {

    val concreteKconfig = KConfigParser.parseKConfigFile(extractFile)
    val s = new FeatureStatistics(concreteKconfig)

    def toStringList(in: List[CSymbol]) = in.map {
      _ match {
        case c: CConfig => prefix + c.name + suffix
        case c: CChoice => sys.error("Nesting of groups not supported. Should it?")
        case x => sys.error("Unknown child of a CChoice: " + x)
      }
    }


    val xor = s.xorGroups map {
      f =>
        FeatureGroup(f.nId.toString, toStringList(f.children), gXOR)
    }

    val or = s.orGroups map {
      f =>
        FeatureGroup(f.nId.toString, toStringList(f.children), gOR)
    }

    val mutex = s.mutexGroups map {
      f =>
        FeatureGroup(f.nId.toString, toStringList(f.children), gMUTEX)
    }

    if (print) {
      printGroups(xor, properties.getProperty(XOR_GROUP_FILE))
      printGroups(or, properties.getProperty(OR_GROUP_FILE))
      printGroups(mutex, properties.getProperty(MUTEX_GROUP_FILE))
    }

    xor ::: or ::: mutex
  }

  def getCDLGroups(imlFile: String, prefix: String, suffix: String, print: Boolean = false) = {

    val es = TSE11Statistics(imlFile)

    val xor = es.xorGroups map {
      case (p, children) =>
        FeatureGroup(p.id, children.map(prefix + _.id + suffix), gXOR)
    }

    val or = es.orGroups map {
      case (p, children) =>
        FeatureGroup(p.id, children.map(prefix + _.id + suffix), gOR)
    }

    val mutex = es.mutexGroups map {
      case (p, children) =>
        FeatureGroup(p.id, children.map(prefix + _.id + suffix), gMUTEX)
    }

    if (print) {
      printGroups(xor, properties.getProperty(XOR_GROUP_FILE))
      printGroups(or, properties.getProperty(OR_GROUP_FILE))
      printGroups(mutex, properties.getProperty(MUTEX_GROUP_FILE))
    }

    xor ::: or ::: mutex
  }


}
