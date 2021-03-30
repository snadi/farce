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
object DimacsComposer {

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

  //backup of all data members describing curr formula
  var idMapBackup = HashMap[String, String]()
  //global id map for whole dimacs. Feature --> id
  var reverseIdMapBackup = HashMap[String, String]()
  // reverse global id map for whole dimacs, id --> Feature
  var clausesBackup = Set[DimacsClause]()
  //global clauses
  var generatedIdsBackup = Set[String]()
  //keep track of which ids are generated to mark them with $
  var lastIdBackup = 0
  var lastGeneratedIdBackup = 0

  val GENERATED_PREFIX = "__fresh"
  var checkForConflicts = false

  def backup {
    idMapBackup = idMap
    reverseIdMapBackup = reverseIdMap
    clausesBackup = clauses
    generatedIdsBackup = generatedIds
    lastIdBackup = lastId
    lastGeneratedIdBackup = lastGeneratedId
  }

  def revertToBackup {
    idMap = idMapBackup
    reverseIdMap = reverseIdMapBackup
    clauses = clausesBackup
    generatedIds = generatedIdsBackup
    lastId = lastIdBackup
    lastGeneratedId = lastGeneratedIdBackup
  }

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

  def isCurrDimacsSat: Boolean = {
    val NBVARS = idMap.size
    val NBCLAUSES = clauses.size
    val solver = SolverFactory.newDefault()
    solver.newVar(NBVARS)
    solver.setExpectedNumberOfClauses(NBCLAUSES)


    for (clause <- clauses) {
      solver.addClause(clause.getIVec)
    }

    solver.isSatisfiable

  }

  def computeDimacs(inputLines: List[String], initialFormulaLimit: Int, outputFile: String){
    var counter = 1
    //add each dimacs file in list
    for (file <- inputLines) {
      println(counter + " " + file)

      if (checkForConflicts && counter > initialFormulaLimit) {
        backup
      }

      addDimacs(file)

      if (checkForConflicts && counter > initialFormulaLimit && !isCurrDimacsSat) {
        revertToBackup
        System.err.println("ERROR: file " + file + " caused contradiction. Reverted")
      }
      counter += 1
    }

    writeFinalDimacs(outputFile)
  }

  def main(args: Array[String]) {

    val inputLines = io.Source.fromFile(args(0)).getLines().toList

    println("Num. of lines read: " + inputLines.size)

    var initialFormulaLimit = 0

    if(args.length > 2 && args(2).equals("checkconflict")){
      checkForConflicts = true

      if(args.length > 3)
        initialFormulaLimit = args(3).toInt
    }


     computeDimacs(inputLines, initialFormulaLimit, args(1))
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

  def addDimacs(dimacsFile: String) {
    var keyMap = HashMap[String, String]() //local map from a new id to an old id

    val source = scala.io.Source.fromFile(dimacsFile)
    for (line <- source.getLines) {
      if (line.startsWith("c")) {
        val parts = line.split(" ")
        val id = parts(1)
        val feature = parts(2).trim()

        if (!id.endsWith("$")) {

          if (idMap.contains(feature)) {
            //if this feature is seen before, then map the new id we see to the old id already in the map
            keyMap += id -> idMap.get(feature).get
          } else {
            //first time to see this feature
            //increment last id and map this feature to it
            lastId += 1
            idMap += feature -> lastId.toString
            reverseIdMap += lastId.toString -> feature

            //for first dimacs added, id should = lastId. If not, add a mapping
            if (id != lastId.toString) {
              keyMap += id -> lastId.toString
            }
          }

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
            //this key is not mapped to a new id and so its value is the same
            if (isNegated) {
              clause.addVariable("-" + variable)
            } else {
              clause.addVariable(variable)
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
