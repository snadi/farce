package gsd.farce.stats

import io.Source
import scala.Predef._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/08/13
 * Time: 1:48 AM
 * To change this template use File | Settings | File Templates.
 */
/*
reads in a file with all the TypeChef times from all files analyzed, and computes
total, avg, and std deviation for each analysis
 */
object TimingStatsCollector {
  var loadFm, interfaces, parsing, lexing, writeInterfaces, typeChecking = Vector[Double]()


  def addToSet(label: String, value: Double) {
    if (label.equals("loadFM")) {
      loadFm = loadFm :+ (value/1000)
    } else if (label.equals("interfaces")) {
      interfaces = interfaces :+ (value/1000)
    } else if (label.equals("parsing")) {
      parsing = parsing :+ (value/1000)
    } else if (label.equals("lexing")) {
      lexing = lexing :+ (value/1000)
    } else if (label.equals("writeInterfaces")) {
      writeInterfaces = writeInterfaces :+ (value/1000)
    } else if (label.equals("typechecking")) {
      typeChecking = typeChecking :+ (value/1000)
    }
  }

  def main(args: Array[String]) = {

    var count = 0
    val reader = Source.fromFile("timingStats").bufferedReader()
    val numOfLines = Source.fromFile("timingStats").getLines().size
    var lineNum = 0

    while (lineNum < numOfLines) {
      var labelLine = reader.readLine().substring(8)
      labelLine = labelLine.substring(0, labelLine.size - 1)
      val labels = labelLine.split(",")

      val statLine = reader.readLine()


      val stats = statLine.split(";")

      assert(stats.size == labels.size, "Problem with line: " + lineNum)
      lineNum += 2

      //timing (loadFM, interfaces, parsing, lexing, writeInterfaces, typechecking)
        for (i <- 0 to stats.length - 1) {
          addToSet(labels(i).trim(), stats(i).toDouble)
        }
    }

    println("Total fm load time: " + getTotalTime(loadFm) + "s")
    println("Total lexing time: " + getTotalTime(lexing)+ "s")
    println("Total parsing time: " + getTotalTime(parsing)+ "s")
    println("Total type checking time: " + getTotalTime(typeChecking)+ "s")
    println("Total interface creation time: " + getTotalTime(interfaces)+ "s")
    println("Total interface write time: " + getTotalTime(writeInterfaces)+ "s")


    println("Avg. lexing time: " + getArithmeticMean(lexing)+ "s")
    println("Avg. parsing time: " + getArithmeticMean(parsing)+ "s")
    println("Avg. type checking time: " + getArithmeticMean(typeChecking) + "s")
    println("Avg. interface creation time: " + getArithmeticMean(interfaces)+ "s")


    println("Stddev lexing time: " + stddev(lexing)+ "s")
    println("Stddev parsing time: " + stddev(parsing)+ "s")
    println("Stddev  type checking time: " + stddev(typeChecking)+ "s")
    println("Stddev  interface creation time: " + stddev(interfaces)+ "s")

  }

  def getTotalTime(inputValues: Vector[Double]) = {
    var total = 0.0

    for (value <- inputValues) total += value

    total
  }

  def getArithmeticMean(inputValues: Vector[Double]) : Float  = {
     var total = 0.0

    for (value <- inputValues) total += value

    (total.toFloat / inputValues.size.toFloat)
  }

  def stddev[T](items:Vector[Double])(implicit n:Numeric[Double]) : Double = {
    math.sqrt(variance(items))
  }

  def variance[T](items:Vector[Double])(implicit n:Numeric[Double]) : Double = {
    val itemMean = getArithmeticMean(items)
    val count = items.size
    val sumOfSquares = items.foldLeft(0.0d)((total,item)=>{
      val itemDbl = n.toDouble(item)
      val square = math.pow(itemDbl - itemMean,2)
      total + square
    })
    sumOfSquares / count.toDouble
  }

}
