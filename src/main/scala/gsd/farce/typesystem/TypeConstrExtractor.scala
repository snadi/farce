package gsd.farce.typesystem

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import java.io.{FileReader, FileWriter, PrintWriter, File}
import scala.xml._
import xml.Node
import gsd.farce.filepcs.FilePCUtils._
import java.util.Properties
import gsd.farce.utilities.PropertyKeys._
import gsd.farce.features.CreateDimacs
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.Utilities._
import de.fosd.typechef.error.Severity

/*
type analyzer which prints type constraints to a single file (not dimacs) -- can be used for small systems
 */
object TypeConstrExtractor {

  def filterFileList(files: List[String], path: String) = {
    files.filter(file => (new File(path + file + ".c.xml")).exists)
  }

  def main(args: Array[String]) = {

    var fileList: List[String] = null
    val config = getSystemConfig(args(0))
    val path = config.getSourceDir
    val properties = new Properties()
    loadPropertiesFile(args(0), properties)

    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

    System.err.println("starting")

    System.err.println("file list size: " + fileList.size)
    fileList = filterFileList(fileList, path)
    System.err.println("filtered file list size: " + fileList.size)

    //get type constraints and type formula
    val (constraints, typeErrorExpr) = getConstraints(fileList, path)

    //print constraints to a file
    val constraintsWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(TYPE_CONSTRAINTS_FILE))))

    constraints.foreach(x => {
      x.print(constraintsWriter)
      constraintsWriter.println()
    })

    constraintsWriter.close()

    System.err.println("Wrote constraints")

    //print formula to file
    if (typeErrorExpr.isTautology()) {
      System.err.println("Type Expression is a tautology")
    } else {

      val errorExprWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(TYPE_FORMULA_FILE))))

      if (!typeErrorExpr.isContradiction()) {
        typeErrorExpr.print(errorExprWriter)
        errorExprWriter.println()
      } else {
        System.err.println("Type Expression is a contradiction!")
      }

      errorExprWriter.close()
    }

    System.err.println("wrote formula")
  }

  def getSeverity(severity: String): Severity.Severity = {
    if (severity.equals("Critical")) {
      Severity.Crash
    } else if (severity.equals("Id-Lookup Error")) {
      Severity.IdLookupError
    } else if (severity.equals("Field-Lookup Error")) {
      Severity.FieldLookupError
    } else if (severity.equals("Type-Lookup Error")) {
      Severity.TypeLookupError
    } else if (severity.equals("Redeclaration Error")) {
      Severity.RedeclarationError
    } else if (severity.equals("Warning")) {
      Severity.Warning
    } else if (severity.equals("Security Warning")) {
      Severity.SecurityWarning
    } else {
      // if (severity.equals("Error")){
      Severity.OtherError
    }

  }

  def getConstraints(fileList: List[String], path: String): (Set[FeatureExpr], FeatureExpr) = {

    var constraints = Set[FeatureExpr]()
    var typeErrorFormula = FeatureExprFactory.True

    var countFailed, countProcessed, countTrue, count, countCrash,countOther,countSecurity, countWarning, countRedeclaration,otherError  = 0

    for (file <- fileList) {
      System.err.println(count + " file: " + file)
      val src = XML.loadFile(path + file + ".c.xml")

      //keep track of seen expressions in each file. Usually, the same expr is repeated in several type errors in the same file
      var seenExpressions = Set[String]()

      val filePC = getFilePC(path + file)

      for (error <- src.child) {
        val errorType = error.label

        //only analyze type errors
        if (errorType.equals("typeerror")) {
          countProcessed += 1
          val featureExprString = (error \\ "featurestr").text.trim()
          val severity = getSeverity((error \\ "severity").text.trim())

          severity match{
            case Severity.IdLookupError | Severity.FieldLookupError | Severity.TypeLookupError  =>{
              if(featureExprString == "True"){
                System.err.println("Type error in file: " + file + " is under condition True")
                countTrue+=1
              } else if (!seenExpressions.contains(featureExprString)) {
                seenExpressions += featureExprString

                val input = new ANTLRInputStream(featureExprString)
                val lexer = new FExprLexer(input)
                val tokens = new CommonTokenStream(lexer)
                val parser = new FExprParser(tokens)
                val featureExpr = parser.fexpr().value

                val constraint = filePC.implies(featureExpr.not())

                val newExpr = constraint.and(typeErrorFormula)

                if (newExpr.isSatisfiable()) {
                  //uncomment if you want to trace files to constraints
                  println(file + ":" + constraint)
                  constraints += constraint
                  typeErrorFormula = newExpr
                } else {
                  System.err.println("Type error in file: " + file + " caused contradiction with constraint: " + constraint)
                  System.err.println("file pc is: " + filePC + " expr is : " + featureExpr)
                  countFailed += 1
                }
              }
            }

            case Severity.Crash => countCrash+=1
            case Severity.OtherError => countOther+=1
            case Severity.SecurityWarning => countSecurity +=1
            case Severity.Warning => countWarning += 1
            case Severity.RedeclarationError => countRedeclaration += 1
            case Severity.OtherError => otherError += 1
          }


        }
      }
      count += 1
    }


    System.err.println("count files:" + fileList.size)
    System.err.println("count processed: " + countProcessed + " type errors")
    System.err.println("extracted " + constraints.size + " type constraints")
    System.err.println("count failed: " + countFailed)
    System.err.println("count true: " + countTrue)
    System.err.println("Count crash, security, warning, redeclaration, other:  " + countCrash + "," + countWarning +"," + countRedeclaration +"," + countOther)

    (constraints, typeErrorFormula)
  }
}
