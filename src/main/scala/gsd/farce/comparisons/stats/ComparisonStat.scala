package gsd.farce.comparisons.stats

import scala.collection.mutable

/**
 * Created by snadi on 24/12/13.
 */
class ComparisonStat(name:String){

  val statName: String = name
  var statsMap = mutable.HashMap[String, String]()
  //val numOfConstraints, numOfTaut, numOfContr, countPreprocessor, countParser, countType, countLinker, countSpec1Added, countFeffect, countFeffect_build, countSpec2Added, countSpec1Combined, countSpec2Combined, countCodeCombined : Int
  //val percPreprocessor, percParser, percType, percLinker, percSpec1Added, percFeffect, percFeffect_build, percSpec2Added, percSpec1Combined, percSpec2Combined, percCodeCombined : Double

  def readStats(file: String){
    val lines = io.Source.fromFile(file).getLines().toList

    for(line <- lines){
      //line format: label, count, percentage
      val parts = line.split(",")

      statsMap += parts(0) + "Count" ->  parts(1).trim()
      statsMap += parts(0) + "Perc" -> parts(2).trim()
    }
  }

   def outputStats{
      statsMap.keysIterator.foreach(key => println("\\pgfkeyssetvalue{" + statName + "_" + key + "}{" + statsMap.get(key).get.trim.toDouble.round + "}"))
    }

  def getStat(name: String): String = statsMap.getOrElse(name,"-1")

}
