package gsd.farce.parser

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import java.io.{FileWriter, PrintWriter, File}
import scala.xml._
import xml.Node
import gsd.farce.filepcs.FilePCUtils._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import java.util.Properties
import gsd.farce.utilities.PropertyKeys._
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 28/01/13
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */

/*
Calculates the pareser error constraints
Takes the name of the system as a parameter
 */

object ParserConstrExtractor {

  val properties = new Properties()

  def main(args: Array[String]) = {

    var errorExpr = FeatureExprFactory.True


    var fileList: List[String] = null
    val config = getSystemConfig(args(0))

    loadPropertiesFile(args(0), properties)

    val path = config.getSourceDir
    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

    System.err.println("file list size: " + fileList.size)

    def filterFileList(files: List[String]) = {
      files.filter(file => (new File(path + file + ".c.xml")).exists)
    }

    System.err.println("starting")
    fileList = filterFileList(fileList)
    System.err.println("filtered file list size: " + fileList.size)

    errorExpr = getErrorExpr(fileList, path)

    //write the overall formula
    val errorExprWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(PARSER_FORMULA_FILE))))

    if (!errorExpr.isTautology()) {
      if (!errorExpr.isContradiction()) {
        errorExpr.print(errorExprWriter)
        errorExprWriter.println()
      } else {
        System.err.println("Final expression is contradiction!")
      }
    } else{
        System.err.println("Final expression is tautology!")
    }

    errorExprWriter.close()

  }


  def getErrorExpr(fileList: List[String], path: String): FeatureExpr = {
    var parserErrorExpr: FeatureExpr = FeatureExprFactory.True

    var countProcessed = 0
    var countFailed = 0
    var parserConstraints = Set[FeatureExpr]()
    var fileCount = 1

    //for each source file we are analyzing, look at its .c.xml file
    for (file <- fileList) {
      try {
        System.err.println("Processing file : " + fileCount + " : " + file)
        fileCount += 1
        val src = XML.loadFile(path + file + ".c.xml")

        val filePc = getFilePC(path + file)

        //only loop over the parser errors
        for (error <- src.child) {
          val errorType = error.label

          if (errorType.equals("parsererror")) {
            countProcessed += 1
            val featureExprString = (error \\ "featurestr").text.trim()

            val input = new ANTLRInputStream(featureExprString)
            val lexer = new FExprLexer(input)
            val tokens = new CommonTokenStream(lexer)
            val parser = new FExprParser(tokens)
            val featureExpr = parser.fexpr().value

            //a parser constraint is the file pc => !errorPC
            val constraint = filePc.implies(featureExpr.not())

            val newFormula = parserErrorExpr.and(constraint)

            if (newFormula.isSatisfiable()){
              println(file + ": "+ constraint)
              parserConstraints += constraint
              parserErrorExpr = newFormula
            }else{
              countFailed = countFailed + 1

              System.err.println("*** FAIL *****")
              System.err.println("file name: " + path + file)
              System.err.println("pc condition: " + filePc)
              System.err.println("feature expr of error: " + featureExpr)
            }
          }
        }
      } catch {
        case e: Exception => {
          System.err.println("Parsing file : " + file + " failed")
          e.printStackTrace()
        }
      }
    }

    System.err.println("count files:" + fileList.size)
    System.err.println("count processed: " + countProcessed + " parser errors")
    System.err.println(countFailed + " FAILED")

    System.err.println("writing to file")
    val constraintsWrtier = new PrintWriter(new File(properties.getProperty(PARSER_CONSTRAINTS_FILE)))
    parserConstraints.foreach(constraint => {
      constraint.print(constraintsWrtier)
      constraintsWrtier.println()
    })

    constraintsWrtier.close()
    System.err.println("wrote " + parserConstraints.size + " constraints")

    //return the overall formula
    parserErrorExpr

  }
}