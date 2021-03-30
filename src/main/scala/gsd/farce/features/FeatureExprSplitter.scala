package gsd.farce.features

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 21/03/13
 * Time: 12:22 PM
 * To change this template use File | Settings | File Templates.
 */
object FeatureExprSplitter extends App {

  for (line <- Source.fromFile(args(0)).getLines()) {
    if (line.startsWith("defined")) {

      val parts = line.split("=>")

      //only attempt breaking lines that have conjunctions but no disjunctions
      if (parts(1).contains("&&") && !parts(1).contains("||")){

        val rightSideParts = parts(1).split("&&")

        rightSideParts.map(x => println(parts(0) + " => " +  x))

      } else{
        println(line)
      }
    }
  }
}
