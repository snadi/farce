package gsd.farce.preprocessor

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import java.io.{FileWriter, PrintWriter, File}
import gsd.farce.linker._
import java.util.Properties
import io.Source
import gsd.farce.utilities.PropertyKeys._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.Config

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 28/01/13
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */


object PreprocessorConstrExtractor extends App {


  val properties = new Properties()

  loadPropertiesFile(args(0), properties)

  var config: Config = null
  var fileList: List[String] = null

  config = getSystemConfig(args(0))

  val path = config.getSourceDir
  fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

  def filterFileList(files: List[String]) = {
    files.filter(file => (new File(path + file + ".hasherr")).exists)
  }

  fileList = filterFileList(fileList)

  var errorExpr = FeatureExprFactory.True
  var readErrors = Set[FeatureExpr]()
  var oldExpr =  FeatureExprFactory.True


  //for each .hasherr file
  for (file <- fileList) {

    //get the feature expression on each line, and add that expression to the overall formula
    //hasherror expressions already come as filepc => !errorPC
    for (line <- Source.fromFile(path + file + ".hasherr").getLines()) {

      val input = new ANTLRInputStream(line.trim())
      val lexer = new FExprLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new FExprParser(tokens)
      val constraint = parser.fexpr().value

      if (!readErrors.contains(constraint)) {
        readErrors += constraint
        oldExpr = errorExpr

        errorExpr = errorExpr.and(constraint)

        //check if the overall formula is still satisfiable
        if(!errorExpr.isSatisfiable()){
          System.err.println("Preprocessor error in file: " + file + " caused contradiction with constraint: " + constraint)
          errorExpr= oldExpr
        }
      }
    }
  }

  //write out the unique constraints so we have them all in one file
  val constraintsWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(PREPROCESSOR_ERROR_CONSRAINT_FILE))))

  readErrors.foreach(constraint => {
    constraint.print(constraintsWriter)
    constraintsWriter.println()
  })

  constraintsWriter.close()

  //write out the final preprocessor expression
  val errorWriter = new PrintWriter(new FileWriter(new File(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE))))

  if(!errorExpr.isContradiction())
    errorWriter.println(errorExpr)
  else{
    System.err.println("error expr is a contradiction!")
  }
  errorWriter.close()
}