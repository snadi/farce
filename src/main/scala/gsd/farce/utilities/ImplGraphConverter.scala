package gsd.farce.utilities

import io.Source
import java.io.{File, FileWriter, PrintWriter}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 26/02/13
 * Time: 5:03 PM
 * To change this template use File | Settings | File Templates.
 */
/*
Converts from implication graph format outputted by fm-translation to the simple format we use here
 */
object ImplGraphConverter extends App {

  var idMap: Map[String, String] = Map()

  val writer = new PrintWriter(new FileWriter(new File(args(1))))

  val prefix = if(args.length > 2) args(2) else ""
  val suffix = if (args.length >3) args(3) else ""

  for (line <- Source.fromFile(args(0)).getLines()) {
    if (line.contains(":")) {
      val parts = line.split(":")
      var featureName = parts(1).trim()

      if(featureName.endsWith(";"))
        featureName = featureName.substring(0, featureName.length - 1)

      idMap += (parts(0).trim() -> (prefix + featureName+ suffix))
    } else {
      val implications = line.split(";")

      for (implication <- implications) {
        if (implication.contains("->")) {
          val parts = implication.split("->")
          val var1 =    idMap.get(parts(0).trim()).get
          val var2 =  idMap.get(parts(1).trim()).get
          if(!var1.contains("__fresh") && !var2.contains("__fresh") && !var1.endsWith("_m" + suffix) && !var2.endsWith("_m" + suffix))
            writer.println(var1 + " => " + var2)
        } else if (implication.contains("--")) {

          val parts = implication.split("--")
          val var1 =    idMap.get(parts(0).trim()).get
          val var2 =  idMap.get(parts(1).trim()).get
          //mutex format: id1--id2
          if(!var1.contains("__fresh") && !var2.contains("__fresh"))
            writer.println(var1 + " -- " + var2)
        }
      }
    }
  }

  writer.close()

}
