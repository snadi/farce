package gsd.farce.comparisons.stats

import gsd.farce.utilities.Utilities
import scala.math._

/**
 * Created by snadi on 24/12/13.
 */
object LatexFullRecoveraStatsGenerator {



  def main(args: Array[String]) {

    var totalRecovered, totalRecovSpecOne, totalRecovSpecTwo = scala.collection.mutable.MutableList[Double]()
    var hierarchySystemStats = Set[ComparisonStat]()
    var crosstreeSystemStats = Set[ComparisonStat]()

    for (system <- Utilities.getSupportedSystems) {
      if (system != "default") {
        System.err.println("Analyzing :" + system)
        var temp = 0.0
        //output individual system stats first
        val hierarchyStats = new ComparisonStat(system + "Hierarchy")
        hierarchyStats.readStats(Utilities.getSystemConfig(system).getHierarchyFullResults)
        hierarchyStats.outputStats
        hierarchySystemStats += hierarchyStats


        val crosstreeStats = new ComparisonStat(system + "Crosstree")
        crosstreeStats.readStats(Utilities.getSystemConfig(system).getCrosstreeFullResults)
        crosstreeStats.outputStats
        crosstreeSystemStats += crosstreeStats


        //then output totals for each system
        temp = getTotal(system, "codeCombinedCount", hierarchyStats, crosstreeStats)
        if (temp >= 0) totalRecovered += temp

        temp = getTotal(system, "spec1CombinedCount", hierarchyStats, crosstreeStats)
        if (temp >= 0) totalRecovSpecOne += temp

        temp = getTotal(system, "spec2CombinedCount", hierarchyStats, crosstreeStats)
        if (temp >= 0) totalRecovSpecTwo += temp

      }
    }


    //get geom means across systems per constraint type for both hierarchy & cross tree
    getTotalsPerConstrType("hierarchy", "codeAddedPerc", hierarchySystemStats)
    getTotalsPerConstrType("hierarchy", "spec1AddedPerc", hierarchySystemStats)
    getTotalsPerConstrType("hierarchy", "spec2AddedPerc", hierarchySystemStats)
    getTotalsPerConstrType("crosstree", "codeAddedPerc", crosstreeSystemStats)
    getTotalsPerConstrType("crosstree", "spec1AddedPerc", crosstreeSystemStats)
    getTotalsPerConstrType("crosstree", "spec2AddedPerc", crosstreeSystemStats)

    //get total combined
    getTotalsPerConstrType("hierarchy", "codeCombinedPerc", hierarchySystemStats)
    getTotalsPerConstrType("hierarchy", "spec1CombinedPerc", hierarchySystemStats)
    getTotalsPerConstrType("hierarchy", "spec2CombinedPerc", hierarchySystemStats)
    getTotalsPerConstrType("crosstree", "codeCombinedPerc", crosstreeSystemStats)
    getTotalsPerConstrType("crosstree", "spec1CombinedPerc", crosstreeSystemStats)
    getTotalsPerConstrType("crosstree", "spec2CombinedPerc", crosstreeSystemStats)


    //calculate total recov geometric means across systems
    println("\\pgfkeyssetvalue{totalrecov}{" + geometricMean(totalRecovered).round + "}")
    println("\\pgfkeyssetvalue{totalrecovspecone}{" + geometricMean(totalRecovSpecOne).round + "}")
    println("\\pgfkeyssetvalue{totalrecovspectwo}{" + geometricMean(totalRecovSpecTwo).round + "}")

  }

  def getTotalsPerConstrType(constrType: String, key: String, stats: Set[ComparisonStat] ){
    var total = scala.collection.mutable.MutableList[Double]()
    for(hierarchyStat <- stats)    {
      val value = hierarchyStat.getStat(key).toDouble
      if(value >= 0)
        total += value
    }

    println("\\pgfkeyssetvalue{total" + constrType + key + "}{" + geometricMean(total).round + "}")
  }

  def geometricMean(list: scala.collection.mutable.MutableList[Double]) = {
    val product = list.map(_ + 1).reduceLeft(_ * _)

    pow(product, (1.0 / list.size.asInstanceOf[Double])) - 1
  }

  /*
  get the total for each system by adding the recovered hierarchy and cross tree constraints and dividing by total num of constraints
   */
  def getTotal(system: String, key: String, hierarchyStats: ComparisonStat, crosstreeStats: ComparisonStat): Double = {
    val totalRecoveredCount = (hierarchyStats.getStat(key).toInt + crosstreeStats.getStat(key).toInt)

    val totalConstraints = (hierarchyStats.getStat("numOfConstraintsCount").toInt + crosstreeStats.getStat("numOfConstraintsCount").toInt)

    if (totalRecoveredCount >= 0) {

      println("\\pgfkeyssetvalue{" + system + "_total" + key + "}{" + totalRecoveredCount + "}")
      val totalRecoveredPerc = Utilities.percentage(totalRecoveredCount, totalConstraints)
      println("\\pgfkeyssetvalue{" + system + "_total" + key.replace("Count", "Perc") + "}{" + totalRecoveredPerc + "}")
      return totalRecoveredPerc
    }

    -1
  }

}
