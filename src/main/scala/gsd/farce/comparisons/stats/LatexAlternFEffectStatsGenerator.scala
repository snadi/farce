package gsd.farce.comparisons.stats

import scala.io.Source
import gsd.farce.utilities.PropertyKeys._
import gsd.farce.utilities.Utilities

/**
 * Created by snadi on 27/12/13.
 */
object LatexAlternFEffectStatsGenerator {

  val analyses = List("NoSingletonFilePC", "NoHeaderSingletonFilePc", "NoHeaderNoSingletonFilePC", "NoHeaderNoSingletonNoFilePC", "SingletonFilePC"
    , "SingletonNoFilePC", "NoHeaderSingletonNoFIlePC", "NoSingletonNoFilePC")

  def main(args: Array[String]) {
    val inputList = Source.fromFile(args(0)).getLines()

    for (line <- inputList) {
      val parts = line.split(",")
      val system = parts(0)
      val directory = parts(1)

      getStats(system, directory)

    }
  }

  def getStats(system: String, directory: String) {

    for (analysis <- analyses) {
      val hierarchyStats = new ComparisonStat(system + "_" + analysis + "_Hierarchy")
      hierarchyStats.readStats(directory + "AlternativeFeatureEffect/" + analysis + "/" + system + "_hierarchy_stats.csv")
      hierarchyStats.outputStats

      val crosstreeStats = new ComparisonStat(system + "_" + analysis +  "Crosstree")
      crosstreeStats.readStats(directory + "AlternativeFeatureEffect/" + analysis + "/" + system + "_crosstree_stats.csv")
      crosstreeStats.outputStats

      //then output totals for each system
      getTotal(system + "_" + analysis, "codeCombinedCount", hierarchyStats, crosstreeStats)

    }
  }

  def getTotal(prefix: String, key: String, hierarchyStats: ComparisonStat, crosstreeStats: ComparisonStat): Double = {
    val totalRecoveredCount = (hierarchyStats.getStat(key).toInt + crosstreeStats.getStat(key).toInt)

    val totalConstraints = (hierarchyStats.getStat("numOfConstraintsCount").toInt + crosstreeStats.getStat("numOfConstraintsCount").toInt)

    if (totalRecoveredCount >= 0) {
      println("\\pgfkeyssetvalue{" + prefix + "_total" + key + "}{" + totalRecoveredCount + "}")
      val totalRecoveredPerc = Utilities.percentage(totalRecoveredCount, totalConstraints)
      println("\\pgfkeyssetvalue{" + prefix + "_total" + key.replace("Count", "Perc") + "}{" + totalRecoveredPerc + "}")

      return totalRecoveredPerc
    }
    println("returning -1")

    -1
  }

}
