package gsd.farce.implications

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/02/13
 * Time: 8:50 AM
 * To change this template use File | Settings | File Templates.
 */
class ImplicationGraph(name: String = "") {

  private var implications = Set[Implication]()


  def addImplication(implication: Implication) = {
    implications += implication
  }

  def getName = name


  def printMap = implications.map(x => println(x.toString))

  def getImplications = implications

  //a simple .contains was not working properly :s so I'm just doing the less intelligent way for now
  def containsImplication(implication: Implication): Boolean = {
    for (impl <- implications)
      if (impl.equals(implication))
        return true

    return false
  }

  def getImplGraphSize = implications.size

  override def toString = {
    var result = ""
    implications.map(x => result += x.toString + "\n")

    //println(result)
    result
  }

  def createGraphFromFile(file: String) = {
    for (line <- Source.fromFile(file).getLines()) {
      if (line.trim().length() > 0 && line.contains("")) {
        var delimeter = ""

        if (line.contains("=>"))
          delimeter = "=>"
        else if (line.contains("->"))
          delimeter = "->"
        else if (line.contains("-->"))
          delimeter = "-->"

        val parts = line.split(delimeter)
        val implication = new Implication(parts(0).trim, parts(1).trim())

        addImplication(implication)
      }
    }

  }

}
