package gsd.farce.filepcs

import scala.io.Source

/**
 * Created by snadi on 30/06/14.
 */
/*
Quick hack to extract info from ecos.db map each package to its directory
 */
object EcosDBExtractor {


  def getMap(file: String): Map[String,String] = {
    var dirMap = Map[String, String]()
    val lines = Source.fromFile(file).getLines()

    while (lines.hasNext) {
      var line = lines.next().trim()
      if (line.startsWith("package ")) {
        var parts = line.split(" ")
        var packageName = ""

        if (parts.length > 1){
          var index = 1
          while(parts(index).trim.length == 0){
            index +=1
          }
          packageName = parts(index).replace("{","").trim() //replacing as a workaround for one case with CYGPKG_IO_PCI
        }

        do {
          line = lines.next()
        } while (line != null && !line.trim().startsWith("directory"))

        if (line != null) {
          line = line.trim()
          //line = directory
          parts = line.split(" ")
          var directory = ""
          if (parts.length > 1){
            var index = 1
            while(parts(index).trim.length == 0){
              index +=1
            }
            directory = parts(index).trim()
          }
          else {
            parts = line.split("\\t")
            if (parts.length > 1){
              var index = 1
              while(parts(index).trim.length == 0){
                index +=1
              }
              directory = parts(index).trim()
            }
          }
          var fullPath = "packages/" + directory + "/current/src/"
          fullPath = fullPath.replaceAllLiterally("//", "/")


          dirMap += (packageName -> fullPath)

          do {
            //skip rest of entry
            line = lines.next().trim()
          } while (line != "}")

        }

      }
    }
    dirMap
  }

  def main(args: Array[String]) {

    val dirMap = getMap(args(0))
    dirMap.foreach(f => println(f._1 + "," + f._2) )

  }
}
