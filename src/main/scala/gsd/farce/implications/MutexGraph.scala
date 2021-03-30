package gsd.farce.implications

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/02/13
 * Time: 8:50 AM
 * To change this template use File | Settings | File Templates.
 */
class MutexGraph(name: String = "") {

  private var edges = Set[MutexEdge]()


  def addEdge(edge: MutexEdge) = {
    edges += edge
  }

  def getName = name


  def printMap = edges.map(x => println(x.toString))

  def getEdges = edges

  //a simple .contains was not working properly :s so I'm just doing the less intelligent way for now
  def containsEdge(toCompare: MutexEdge): Boolean = {
    for (edge <- edges)
      if (edge.equals(toCompare))
        return true

    return false
  }

  def getImplGraphSize = edges.size

  override def toString = {
    var result = ""
    edges.map(x => result += x.toString + "\n")

    //println(result)
    result
  }

  def createGraphFromFile(file: String) = {
    for (line <- Source.fromFile(file).getLines()) {
      if (line.trim().length() > 0 && line.contains("")) {
        val parts = line.split("--")
        val edge = new MutexEdge(parts(0).trim, parts(1).trim())

        addEdge(edge)
      }
    }

  }

}
