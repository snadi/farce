package gsd.farce.features

import collection.immutable.HashMap
import java.io.{File, FileWriter, PrintWriter}
import org.sat4j.specs.{IProblem, ISolver}
import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.{DimacsReader, Reader}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 24/07/13
 * Time: 2:21 PM
 * To change this template use File | Settings | File Templates.
 */
/*
Creates a single dimacs composed of two or dimacs
 */
object DimacsVarAdjuster {

  var idMap = HashMap[String, String]()
  //global id map for whole dimacs. Feature --> id
  var reverseIdMap = HashMap[String, String]()
  // reverse global id map for whole dimacs, id --> Feature
  var clauses = Set[DimacsClause]()
  //global clauses
  var generatedIds = Set[String]()
  //keep track of which ids are generated to mark them with $
  var lastId = 0
  var lastGeneratedId = 0

  val GENERATED_PREFIX = "__fresh"

  def writeFinalDimacs(outputFile: String) {
    //write output dimacs file
    val typeWriter = new PrintWriter(new FileWriter(new File(outputFile)))
    for (feature <- idMap.keySet) {
      val id = idMap.get(feature).get
      if (generatedIds.contains(id))
        typeWriter.println("c " + id + "$ " + feature)
      else
        typeWriter.println("c " + id + " " + feature)
    }

    typeWriter.println("p " + idMap.size + " " + clauses.size)

    clauses.foreach(typeWriter.println(_))
    typeWriter.close()
  }

  def main(args: Array[String]) {

   readDimacs(args(0))
    writeFinalDimacs(args(1))
  }

  def containsValue(literal: String, variables: HashMap[String, String]) =
    if (literal.startsWith("-"))
      variables.contains(literal.substring(1))
    else
      variables.contains(literal)

  def lookupLiteral(literal: String, variables: HashMap[Int, String]) =
    if (literal.startsWith("-"))
      "!" + variables.getOrElse(literal.substring(1).toInt, "")
    else
      variables.getOrElse(literal.toInt, "")

  def readDimacs(dimacsFile: String) {
    var keyMap = HashMap[String, String]() //local map from a new id to an old id

    val source = scala.io.Source.fromFile(dimacsFile)
    for (line <- source.getLines) {
      if (line.startsWith("c")) {
        val parts = line.split(" ")
        val id = parts(1)
        val feature = parts(2).trim()

        if (!id.endsWith("$")) {

//          if (idMap.contains(feature)) {
//            //if this feature is seen before, then map the new id we see to the old id already in the map
//            keyMap += id -> idMap.get(feature).get
//          } else {
            //first time to see this feature
            //increment last id and map this feature to it
            lastId += 1
            idMap += feature -> lastId.toString
            reverseIdMap += lastId.toString -> feature
            keyMap += id -> lastId.toString

            //for first dimacs added, id should = lastId. If not, add a mapping
//            if (id != lastId.toString) {
//              keyMap += id -> lastId.toString
//            }
          //}

        } else {
          //always create a new variable for generated variables, and map the old id to the new id
          //change the name of the generated variable to avoid overlap with names in other files

          //generate a new name for variable
          lastGeneratedId += 1
          val newGeneratedName = GENERATED_PREFIX + lastGeneratedId

          //create a new id, and map to new variable name. Add this id to generatedIds set for proper printing later
          lastId += 1
          idMap += newGeneratedName -> (lastId.toString)
          reverseIdMap += (lastId.toString) -> newGeneratedName
          generatedIds += lastId.toString

          //map the original id (without the $) to the new variable created
          keyMap += id.substring(0, id.length - 1) -> lastId.toString
        }

      } else if (!line.startsWith("p")) {
        //the clauses
        val clause = new DimacsClause
        val parts = line.split(" ")

        var index = 0
        while (index < parts.length - 1) {
          var variable = parts(index)
          var isNegated = false

          if (variable.startsWith("-")) {
            variable = parts(index).substring(1)
            isNegated = true
          }

          if (containsValue(variable, keyMap)) {
            //this key is mapped to something else
            if (isNegated)
              clause.addVariable("-" + keyMap.get(variable).get)
            else
              clause.addVariable(keyMap.get(variable).get.toString)
          } else {
            //WORKAROUND: there are some variables which don't have a definition
            //I'm not sure how these came, but we need to adjust their ids as well
            lastId += 1
            keyMap += variable -> lastId.toString
            if (isNegated) {
              clause.addVariable("-" + lastId)
            } else {
              clause.addVariable(lastId.toString)
            }
          }

          index += 1
        }

        clauses += clause
      }

    }

    source.close()
  }
}
