package gsd.farce.typesystem

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import java.io.{FileWriter, PrintWriter, File}
import scala.xml._
import gsd.farce.filepcs.FilePCUtils._
import java.util.Properties
import gsd.farce.features.{TrackedTypeConstraint, CreateDimacs}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr
import gsd.farce.utilities.Utilities._
import de.fosd.typechef.error.Severity
import gsd.farce.utilities.PropertyKeys._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 28/01/13
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */

/*
Can be used for big systems who generate a lot of type constraints that cannot be easily aggregated
creates the type constraints but prints each individual constraint to a dimacs file
args(0): system name
args(1): the directory to which to output the dimacs files (will output to dir/DimacsConstraints
You should run the DimacsComposer after this
*/
object TypeConstrExtractorBigSys {

  def filterFileList(files: List[String], path: String) = {
    files.filter(file => (new File(path + file + ".c.xml")).exists)
  }

  def main(args: Array[String]) = {

    var fileList: List[String] = null
    val config = getSystemConfig(args(0))
    val path = config.getSourceDir
    val properties = new Properties()
    val dir = args(1)
    loadPropertiesFile(args(0), properties)

    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

    System.err.println("starting")

    System.err.println("file list size: " + fileList.size)
    fileList = filterFileList(fileList, path)
    System.err.println("filtered file list size: " + fileList.size)

    //get type constraints in a set to avoid duplicates
    val constraints = getConstraints(fileList, path)
    var countConstraint = 0

    val typeTrackingWriter = new PrintWriter(new FileWriter(new File("output/TypeErrors/typeSources")))
    val typeConstraintsWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(TYPE_CONSTRAINTS_FILE))))
    //convert each constraint into dimacs and print it
    for (constraint <- constraints) {
      try {


        countConstraint += 1
        //label constraints so we know their severity
        CreateDimacs.createDimacs(constraint.getConstraint.asInstanceOf[SATFeatureExpr], dir + "/" + constraint.getLabel + countConstraint + ".dimacs", false, config.getPrefix, config.getSuffix)
        typeTrackingWriter.println("file: " + constraint.getFile + ", constraint: " + constraint.getLabel + countConstraint)
        constraint.getConstraint.print(typeConstraintsWriter)
        typeConstraintsWriter.println
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
    typeConstraintsWriter.close()
    typeTrackingWriter.close()
    System.err.println("Wrote constraints")
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

  def getConstraints(fileList: List[String], path: String): Set[TrackedTypeConstraint] = {
    var countProcessed = 0
    var constraints = Set[TrackedTypeConstraint]()

    var count = 1
    for (file <- fileList) {
      System.err.println(count + " file: " + file)
      val src = XML.loadFile(path + file + ".c.xml")

      //keep track of seen expressions in each file. Usually, the same expr is repeated in several type errors in the same file
      var seenExpressions = Set[String]()

      val pcCondition = getFilePC(path + file)
      for (error <- src.child) {
        val errorType = error.label
        val severity = getSeverity((error \\ "severity").text.trim())

        System.out.println("severity: " + severity)
        //ignore type errors of type "Error" as these may contain false positives
        if (errorType.equals("typeerror")) {
          countProcessed += 1
          val featureExprString = (error \\ "featurestr").text.trim()

          if (!seenExpressions.contains(featureExprString)) {
            seenExpressions += featureExprString

            if (featureExprString.trim().equals("True") || featureExprString.trim().equals("TRUE")) {
              System.err.println("WARNING IGNORING File:" + file + " has TRUE")
            } else {
              val input = new ANTLRInputStream(featureExprString)
              val lexer = new FExprLexer(input)
              val tokens = new CommonTokenStream(lexer)
              val parser = new FExprParser(tokens)
              val featureExpr = parser.fexpr().value


              val constraint = pcCondition.implies(featureExpr.not())

              constraints += new TrackedTypeConstraint(file, constraint, severity)
            }
          }
        }
      }
      count += 1
    }


    System.err.println("count files:" + fileList.size)
    System.err.println("count processed: " + countProcessed + " type errors")
    System.err.println("count constraints: " + constraints.size)

    constraints
  }
}