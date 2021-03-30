//adapted from TypeChef-BusyBoxAnalysis

package gsd.farce.linker

import java.io.{FileWriter, PrintWriter, File}
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExprParser, FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.typesystem.linker._
import java.io.File
import java.util.Properties
import gsd.farce.utilities.PropertyKeys._
import de.fosd.typechef.featureexpr.sat.SATFeatureExprFactory
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.Config


object GlobalLinker extends LinkerUtils {
  var blackList = Set[String]()
  var fileList: List[String] = null
  var interfaces = List[CInterfaceWrapper]()
  var imports = Seq[CSignatureWrapper]()
  var exports = Seq[CSignatureWrapper]()
  var vm: FeatureModel = null
  val properties = new Properties()
  var constraints = Set[FeatureExpr]()

  def linkTreewise(l: List[CInterface]): CInterface = {
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


    if (!config.getFeatureModelFile.endsWith(".dimacs")) {
      featureModel = getVM(config)
      vm = FeatureExprFactory.default.featureModelFactory.create(featureModel)
    } else {
      vm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFile(config.getFeatureModelFile)
    }

    var finalInterface: CInterface = EmptyInterface

    finalInterface = linkTreewise(interfaces)
    blackList -= "match_fstype"
    finalInterface = finalInterface.andFM(featureModel).pack

    finalInterface.imports.foreach(f => blackList += f.name)

    val whilteListFile = new File("output/Linker/whitelist")

    if (whilteListFile.exists()) {
      scala.io.Source.fromFile(whilteListFile).getLines().foreach(f => blackList -= f)
    }

  }

  def getDefUseConstraints(printWriter: PrintWriter) {

    var countFailed = 0

    var countImport = 0
    for (imported <- imports) {
      countImport += 1
      val relatedExports = findRelatedExports(imported, exports)


      val constraint = imported.fexpr.implies(relatedExports)
      if (!constraints.contains(constraint)) {
        if (!constraint.isContradiction()) {
          constraints += constraint
          if (!constraint.isTautology()) {
            //uncomment if you want to trace constraints to imports
            //println(imported.getFile + " , import: " + imported.name + " : " + constraint)
            constraint.print(printWriter)
            printWriter.println()
          }
        } else {
          System.err.println("ERROR file " + imported.getFile + " contains CONTRADICTION for constraint: " + constraint)
          countFailed += 1
        }
      }

    }

    System.err.println("finished creating defUse constraints and had " + countFailed + " failures")

  }

  def main(args: Array[String]) {

    //FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

    val config = getSystemConfig(args(0))
    loadPropertiesFile(args(0), properties)

    //read interfaces
    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList
    System.err.println(fileList.size + " files")
    fileList = filterFileList(fileList, config)
    interfaces = readInterfaces(config)
    System.err.println("read interfaces")

    createBlacklist(config)
    System.err.println("created blacklist size: " + blackList.size)

    exports = getExports(interfaces)
   // exports.foreach(x => println("export: " + x.getFile + ":" + x.name + " : " + x.fexpr))
    imports = getImports(interfaces, exports).filter(f => !blackList.contains(f.name))
   // imports.foreach(x => println("import: " + x.getFile + ":" + x.name + " : " + x.fexpr))

    System.err.println("INFO: num of imports: " + imports.size)
    System.err.println("INFO: num of exports: " + exports.size)


    val printWriter = new PrintWriter(new FileWriter(properties.getProperty(DEFUSE_CONSTRAINTS_FILE)))

    getDefUseConstraints(printWriter)
    getConflictConstraints(printWriter)

    printWriter.close()

    val linkerFExpr = getOverallLinkerFormula
    val formulaWriter = new PrintWriter(new FileWriter(properties.getProperty(DEFUSE_FORMULA_FILE)))
    linkerFExpr.print(formulaWriter)
    formulaWriter.println()
    formulaWriter.close()
  }

  def getOverallLinkerFormula = {
    var linkerFormula = SATFeatureExprFactory.True

    for (constraint <- constraints) {
      val newFormula = linkerFormula.and(constraint)

      if (newFormula.isSatisfiable()) {
        linkerFormula = newFormula
      } else {
        System.err.println("CONTRADICTION with constraint: " + constraint)
      }
    }

    linkerFormula

  }

  def getConflictConstraints(printWriter: PrintWriter) {

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

          if (!constraint.isContradiction()) {
            if (!constraint.isTautology()) {
              if (!constraints.contains(constraint)) {
                constraints += constraint
                //uncomment if you want to tie conflict constraits to export
                //println(exportToCompare.getFile + " , export: " + exportToCompare.name + " : " + constraint)
                constraint.print(printWriter)
                printWriter.println()
              }
            }
          } else {
            System.err.println("ERROR export constraint is contradiction: " + constraint + " original: " + relatedExports.toList)
            countFailed += 1
          }

        }
      }
    }

    System.err.println("finished analyzing conflicts, and had " + countFailed + " failures")
  }
}

