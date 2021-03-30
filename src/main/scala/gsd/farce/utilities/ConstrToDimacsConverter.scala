package gsd.farce.utilities

import io.Source
import de.fosd.typechef.featureexpr.FeatureExpr
import java.util.Properties
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.features.CreateDimacs
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 25/02/13
 * Time: 2:33 PM
 * To change this template use File | Settings | File Templates.
 */
/*
Converts constraint files into dimacs files
takes
1) input list of constraint files
 2) directory to which to output converted dimacs files
 */
object ConstrToDimacsConverter extends App {


  var linesProcessed = Set[String]()
  //val properties = new Properties()

  //loadPropertiesFile(args(0), properties)

  val config = getSystemConfig(args(0))
  val outputDir = args(2)

  var count = 1
  var constraints = Set[FeatureExpr]()


  var fileList = Source.fromFile(args(1)).getLines().toList
  println("filelist size:" + fileList.size)


  println("Num of constraints: " + fileList.size)

  for (file <- fileList) {
    println("Line: " + count + " file: " + file)

    val constraintName = file.substring(file.lastIndexOf("/") + 1)
    //constraintNum = constraintNum.substring(10, constraintNum.length - 4)
    val line = Source.fromFile(file).getLines().next().trim

    val input = new ANTLRInputStream(line.trim())
    val lexer = new FExprLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new FExprParser(tokens)
    val constraint = parser.fexpr().value

    CreateDimacs.createDimacs(constraint.asInstanceOf[SATFeatureExpr], outputDir + "/" + constraintName + ".dimacs", false, config.getPrefix, config.getSuffix)

  }

  System.err.println("Created dimacs for all ")
}
