//adapted from TypeChef-BusyBoxAnalysis

package gsd.farce.linker

import java.io.{FileWriter, PrintWriter, File}
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExprParser, FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.typesystem.linker._
import java.io.File
import java.util.Properties
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.sat.{SATFeatureExpr, SATFeatureExprFactory}
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.Config
import gsd.farce.features.{DimacsComposer, TrackedTypeConstraint, TrackedLinkerConstraint, CreateDimacs}


object GlobalLinkerBigSys extends LinkerUtils {
  var blackList = Set[String]()
  var fileList: List[String] = null
  var interfaces = List[CInterfaceWrapper]()
  var imports = Seq[CSignatureWrapper]()
  var exports = Seq[CSignatureWrapper]()
  var vm: FeatureModel = null
  val properties = new Properties()
  var constraints = Set[TrackedLinkerConstraint]()

  def linkTreewise(l: List[CInterface]): CInterface = {
    System.err.println("link tree wise with l.size: " + l.size)
    if (l.size > 2) {
      val m: Int = l.size / 2
      val left = l.take(m)
      val right = l.drop(m)
      linkTreewise(List(linkTreewise(left), linkTreewise(right)))
    }
    else if (l.size == 2) {
      val left = l(0)
      val right = l(1)

      val confl = left getConflicts right
      for (c <- confl)
        if (!c._2.isTautology(vm)) {
          c._3.foreach(s => blackList += s.name)
        }
      if (!(left isCompatibleTo right)) {
        println(confl)
        for (c <- confl)
          c._3.foreach(s => blackList += s.name)
      }
      left link right
    } else if (l.size == 1) l(0)
    else {
      //  assert(false, l)
      EmptyInterface
    };

  }


  def linkIncrementally(l: List[CInterface]): CInterface = l.fold(EmptyInterface)((left, right) => {
    if (!(left isCompatibleTo right))
      println(left getConflicts right)
    left link right
  })

  def filterFileList(files: List[String], config: Config) = {
    files.filter(file => (new File(config.getSourceDir + file + config.getInterfaceExtension)).exists)
  }

  def readInterfaces(config: Config) = {
    val reader = new InterfaceWriter() {}

    fileList.map(f => {
      System.err.println("analyzing file: " + f)
      new CInterfaceWrapper(config.getSourceDir + f + ".c", reader.readInterface(new File(config.getSourceDir + f + config.getInterfaceExtension)))
    }).map(w => new CInterfaceWrapper(w.getFile, SystemLinker.linkStdLib(w)))
  }

  def createBlacklist(config: Config) {
    var featureModel: FeatureExpr = null
    System.err.println("creating black list")

    if (!config.getFeatureModelFile.endsWith(".dimacs")) {
      System.err.println("doesn't end with dimacs")
      featureModel = getVM(config)
      vm = FeatureExprFactory.default.featureModelFactory.create(featureModel)
    } else {
      System.err.println("ends with dimacs")
      vm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFile(config.getFeatureModelFile)
    }

    var finalInterface: CInterface = EmptyInterface

    finalInterface = linkTreewise(interfaces)

    System.err.println("finished linking")
    finalInterface = finalInterface.andFM(featureModel).pack
    System.err.println("finished packing")
    finalInterface.imports.foreach(f => blackList += f.name)
    System.err.println("updated blacklist")
  }

  def getDefUseConstraints() {

    var countFailed = 0

    var countImport = 0
    var countBlacklist = 0
    for (imported <- imports) {
      val relatedExports = findRelatedExports(imported, exports)

      if (relatedExports != null) {
        countImport += 1
        val constraint = imported.fexpr.implies(relatedExports)
        val trackedConstraint = new TrackedLinkerConstraint(imported.getFile, constraint, imported.name, "defUse")
        if (!constraints.contains(trackedConstraint)) {
          if (!constraint.isContradiction()) {
            if (!constraint.isTautology()) {
              constraints += trackedConstraint
              //uncomment if you want to trace constraints to imports
              //println(imported.getFile + " , import: " + imported.name + " : " + constraint)
              /*constraint.print(printWriter)
              printWriter.println()*/
            }
          } else {
            System.err.println("ERROR file " + imported.getFile + " contains CONTRADICTION for constraint: " + constraint)
            countFailed += 1
          }
        }

      }  else{
             countBlacklist +=1
      }
    }
    System.err.println("finished creating defUse constraints and had " + countFailed + " failures, and " + countBlacklist + " blacklisted")
  }

  def main(args: Array[String]) {

    //FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

    val config = getSystemConfig(args(0))
    loadPropertiesFile(args(0), properties)

    val dir = args(1)

    //read interfaces
    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList
    System.err.println(fileList.size + " files")
    fileList = filterFileList(fileList, config)
    interfaces = readInterfaces(config)
    System.err.println("read interfaces")

    //createBlacklist(config)
    val blackListLines = io.Source.fromFile("output/TypeChefAnalysis/blacklist.txt").getLines().toList
    blackListLines.foreach(b => blackList += b.trim)
    System.err.println("created blacklist size: " + blackList.size)

    exports = getExports(interfaces)
    imports = getImports(interfaces).filter(f => !blackList.contains(f.name))

    System.err.println("INFO: num of imports: " + imports.size)
    System.err.println("INFO: num of exports: " + exports.size)

    getDefUseConstraints()
    System.err.println("INFO: got defuse constraints")
    getConflictConstraints()
    System.err.println("INFO: got conflict constraints")

    val linkerTrackingWriter = new PrintWriter(new FileWriter(new File("output/Linker/linkerSources")))
    var linkerConstraintsWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(DEFUSE_CONSTRAINTS_FILE))))
    var countConstraint = 0
    var constraintList = List[String]()

    //convert each constraint into dimacs and print it
    for (constraint <- constraints) {
      try {
        countConstraint += 1
        //label constraints so we know their severity
        CreateDimacs.createDimacs(constraint.getConstraint.asInstanceOf[SATFeatureExpr], dir + "/" + constraint.getConstraintType + countConstraint + ".dimacs", false, config.getPrefix, config.getSuffix)
        constraintList ::= dir + "/" + constraint.getConstraintType + countConstraint + ".dimacs"
        linkerTrackingWriter.println("file: " + constraint.getFile + ", constraint: " + constraint.getConstraintType + countConstraint + ", symbol: " + constraint.getSymbol)
        constraint.getConstraint.print(linkerConstraintsWriter)
        linkerConstraintsWriter.println()
      } catch {
        case e: java.lang.AssertionError => {
          System.err.println("Assertion for constraint " + constraint + " failed")
          e.printStackTrace()
        }
        case e: Exception => {
          System.err.println("Exception for constraint: " + constraint)
          e.printStackTrace()
        }
      }
    }

    linkerConstraintsWriter.close()
    linkerTrackingWriter.close()

    System.err.println("INFO: finished writing out " + constraintList.size + " constraints")
    System.err.println("INFO: Creating formula through dimacs composer")
    //create formula by dimacs composer
    DimacsComposer.computeDimacs(constraintList, 0, properties.getProperty(DEFUSE_DIMACS_FILE))

    System.err.println("INFO: created formula")
  }

  def getConflictConstraints() {

    //If x is exported under A,B, and C then A => !B & !C, B => !A & !C and C => !A & !B
    //will use atMostOneOf from TypeChef's expr library
    var countFailed = 0
    val exportsArray = exports.toArray
    val featureExprParser = new FeatureExprParser()
    for (i <- 0 until exportsArray.length) {
      val exportToCompare = exportsArray(i)
      //if this export has not already been analyzed, find other exports with same signature
      if (!exportToCompare.alreadyAnalyzed) {
        var relatedExports = Set[FeatureExpr]()

        //add current export to list of exports
        relatedExports += exportToCompare.fexpr

        //then find the related exports
        for (j <- i + 1 until exportsArray.length) {
          if (hasSameName(exportToCompare, exportsArray(j))) {
            exportsArray(j).alreadyAnalyzed = true
            relatedExports += exportsArray(j).fexpr
          }
        }

        if (relatedExports.size > 1) {
          val constraint = featureExprParser.atMostOne(relatedExports.toList)
          val trackedConstraint = new TrackedLinkerConstraint(exportToCompare.getFile, constraint, exportToCompare.name, "conflict")
          if (!constraints.contains(trackedConstraint)) {
            if (!constraint.isContradiction()) {
              if (!constraint.isTautology()) {
                constraints += trackedConstraint
                /* //uncomment if you want to tie conflict constraits to export
                // println(exportToCompare.getFile + " , export: " + exportToCompare.name + " : " + constraint)
                 constraint.print(printWriter)
                 printWriter.println()*/
              }
            } else {
              System.err.println("ERROR export constraint is contradiction: " + constraint + " original: " + relatedExports.toList)
              countFailed += 1
            }
          }

        }
      }
    }

    System.err.println("finished analyzing conflicts, and had " + countFailed + " failures")
  }
}

