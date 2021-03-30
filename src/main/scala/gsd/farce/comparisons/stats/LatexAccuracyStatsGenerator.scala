package gsd.farce.comparisons.stats

import gsd.farce.utilities.Utilities
import scala.math._

/**
 * Created by snadi on 24/12/13.
 */
object LatexAccuracyStatsGenerator {

  def main(args: Array[String]) {

    var accuracySystemStats = Set[ComparisonStat]()
    var totalAccuracy, totalAccSpecOne, totalAccSpecTwo = scala.collection.mutable.MutableList[Double]()


    for (system <- Utilities.getSupportedSystems) {
      if (system != "default") {
        //output individual system stats first
        val accuracyStats = new ComparisonStat(system)
        accuracyStats.readStats(Utilities.getSystemConfig(system).getAccuracyResults)
        accuracyStats.outputStats
        totalAccuracy += accuracyStats.getStat("totalAccuracyPerc").toDouble
        totalAccSpecOne += accuracyStats.getStat("totalAccuracySpeconePerc").toDouble
        totalAccSpecTwo += accuracyStats.getStat("totalAccuracySpectwoPerc").toDouble
      }
    }

    //calculate geometric means across systems
    println("\\pgfkeyssetvalue{totalAccuracy}{" + geometricMean(totalAccuracy).round + "}")
    println("\\pgfkeyssetvalue{totalAccuracySpecone}{" + geometricMean(totalAccSpecOne).round + "}")
    println("\\pgfkeyssetvalue{totalAccuracySpectwo}{" + geometricMean(totalAccSpecTwo).round + "}")

  }

  def geometricMean(list: scala.collection.mutable.MutableList[Double]) = {
    val product = list.reduceLeft(_ * _)

    pow(product, (1.0 / list.size.asInstanceOf[Double]))
  }

}
