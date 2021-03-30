
package gsd.farce.comparisons

import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import sat.{SATFeatureExpr, SATFeatureModel}
import gsd.farce.utilities.PropertyKeys._
import java.io._
import java.util.Properties
import scala.Array
import gsd.farce.implications.ImplicationGraph
import io.Source
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 12/02/13
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */

/*
  Checks if the feature model implications directly extracted from Kconfig (featureModel file) are satisfiable in the
    formulas extracted from the code from the four sources of information: parsing/type typesystem model,
    linker information, nested ifdefs, and errors.

  Args: 1) system to compare
  2) constraints file to compare
  3) output directory prefix
  4) optional "combined" to compare big conjunction. Default: individual.

  Output:
  1) CSV file with each constraint and which formula(s) it was found in
  2) CSV file with stats
  3) text file with a list of constraints not found
  Names of files sysName_constraintNames_description. E.g., busybox_hierarchy_comparison.csv, busybox_hierarchy_stats.csv, busybox_hierarchy_notfound.txt
   */

object FeatureModelComparer {

  /*
   * counting variables
   */

  //individual and unique added counts
  var countPreprocessor = 0
  var countParser = 0
  var countType = 0
  var countLinker = 0
  var countSpec1Added = 0
  var countFeatureEffect = 0
  var countFEffect_Build = 0
  var countSpec2Added = 0
  var countCodeAdded = 0

  //combined counts
  var countSpec1Combined = 0
  var countSpec2Combined = 0
  var countCodeCombined = 0

  var countAll = 0
  var countFound = 0
  var countNotFound = 0


  //flags
  var classify = false
  var testFilePc = true
  var testDefUse = true
  var testParser = true
  var testType = true
  var testNested = true
  var testErrors = true
  var compareCombined = false

  var outputLine = ""

  var defUseExpr, parserExpr, errorsExpr, nestedExpr, typeExpr, bigConjunction, spec1, spec2, filePcExpr: FeatureExpr = null
  var defUseFeatures, parserFeatures, errorsFeatures, nestedFeatures, typeFeatures, bigConjunctionFeatures, spec1Features, spec2Features, filePcFeatures: Set[String] = null
  var comparisonWriter, statsWriter, notFoundWriter: PrintWriter = null

  val properties = new Properties()

  def main(args: Array[String]) {

    try {

      loadPropertiesFile(args(0), properties)

      val constraintsFile = args(1)
      val constraintType = getConstraintType(constraintsFile)
      val outputDir = args(2)

      comparisonWriter = new PrintWriter(new FileWriter(outputDir + "/" + args(0) + "_" + constraintType + "_comparison.csv"))
      statsWriter = new PrintWriter(new FileWriter(outputDir + "/" + args(0) + "_" + constraintType + "_stats.csv"))
      notFoundWriter = new PrintWriter(new FileWriter(outputDir + "/" + args(0) + "_" + constraintType + "_notfound.txt"))



      if (args.length > 3 && args(3).equals("combined")) {
        compareCombined = true
      }

      notFoundWriter.println("going to read")
      readFormulas
      notFoundWriter.println("compared formulas")
      compareConstraints(constraintsFile, compareCombined)

    } finally {
      comparisonWriter.close()
      statsWriter.close()
      notFoundWriter.close()
    }
  }

  def getConstraintType(input: String) = {
    var constraintType = ""
    if (input.contains("/")) {
      constraintType = input.substring(input.lastIndexOf("/") + 1)
    }

    constraintType = constraintType.substring(0, constraintType.indexOf(".constraints"))

    constraintType
  }


  def readFormulas {
    //get feature expressions for all code formulas

    //Def Use
    var input = new ANTLRInputStream(Source.fromFile(properties.getProperty(DEFUSE_FORMULA_FILE)).getLines().next().trim())
    var lexer = new FExprLexer(input)
    var tokens = new CommonTokenStream(lexer)
    var parser = new FExprParser(tokens)
    defUseExpr = parser.fexpr().value
    defUseFeatures = defUseExpr.collectDistinctFeatures
    notFoundWriter.println("INFO: Read defuse")


    val lines = Source.fromFile("output/FilePcs/filePcFormula.txt").getLines()
    if (!lines.isEmpty) {
      input = new ANTLRInputStream(lines.next().trim())
      lexer = new FExprLexer(input)
      tokens = new CommonTokenStream(lexer)
      parser = new FExprParser(tokens)
      filePcExpr = parser.fexpr().value //.parseFile(properties.getProperty(DEFUSE_FORMULA_FILE))
      filePcFeatures = filePcExpr.collectDistinctFeatures
    }

    notFoundWriter.println("INFO: Read filePc")
    if (filePcExpr == null) {
      notFoundWriter.println("INFO: No file pc ")
      testFilePc = false
    } else if (filePcExpr.isTautology()) {
      notFoundWriter.println("INFO: File pc is tautology, will not check against it ")
      testFilePc = false
    }

    //Parser
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PARSER_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    parserExpr = parser.fexpr().value
    parserFeatures = parserExpr.collectDistinctFeatures
    notFoundWriter.println("INFO: read parser")



    //Preprocessor errors
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(PREPROCESSOR_ERROR_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    errorsExpr = parser.fexpr().value
    errorsFeatures = errorsExpr.collectDistinctFeatures
    notFoundWriter.println("INFO: read preproc errors")


    //type system
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(TYPE_FORMULA_FILE)).getLines().next().trim())
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    typeExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(TYPE_FORMULA_FILE))
    typeFeatures = typeExpr.collectDistinctFeatures
    notFoundWriter.println("INFO: read type")

    //Nested ifdef
    input = new ANTLRInputStream(Source.fromFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)).getLines().next().trim)
    lexer = new FExprLexer(input)
    tokens = new CommonTokenStream(lexer)
    parser = new FExprParser(tokens)
    nestedExpr = parser.fexpr().value //featureExprParser.parseFile(properties.getProperty(NESTED_IFDEF_FORMULA_FILE))
    notFoundWriter.println("INFO: parsed nesting expression")
    nestedFeatures = nestedExpr.collectDistinctFeatures


    if (compareCombined) {

//      val part1 = parserExpr.and(typeExpr)
//      if(part1.isContradiction()) notFoundWriter.println("contra between parser and type")
//
//      val part2 = parserExpr.and(defUseExpr).and(errorsExpr)
//      if(part2.isSatisfiable()) notFoundWriter.println("all but type ok")

      spec1 = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr)

      spec1Features = spec1.collectDistinctFeatures
      spec2 = nestedExpr.and(filePcExpr)
      spec2Features = spec2.collectDistinctFeatures
      bigConjunction = errorsExpr.and(parserExpr).and(typeExpr).and(defUseExpr).and(nestedExpr)
      bigConjunctionFeatures = bigConjunction.collectDistinctFeatures
      notFoundWriter.println("created formula")
      if(bigConjunction.isTautology()){
        notFoundWriter.println("WARNING: Big conjunction is a tautology! No need for comparison")
        compareCombined = false
      }else if (bigConjunction.isContradiction()) {
        notFoundWriter.println("WARNING:Big conjunction is a contradiction! will not check against it")
        compareCombined = false
      }
    }
  }


  def compareSingleExpr(line: String, testExpr: FeatureExpr, isGroup: Boolean): Boolean = {
    outputLine = line + ","
    val testFeatures = testExpr.collectDistinctFeatures

    var foundSpec1 = false
    var foundSpec2 = false

    //test for implication in each of the four models and alter output line accordingly

    if (testErrors) {
      if (testFeatures.subsetOf(errorsFeatures) && errorsExpr.implies(testExpr).isTautology()) {
        foundSpec1 = true
        countPreprocessor += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testParser) {
      if (testFeatures.subsetOf(parserFeatures) && parserExpr.implies(testExpr).isTautology()) {
        foundSpec1 = true
        countParser += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testType) {
      if (testFeatures.subsetOf(typeFeatures) && typeExpr.implies(testExpr).isTautology()) {
        foundSpec1 = true
        countType += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testDefUse) {
      if (testFeatures.subsetOf(defUseFeatures) && defUseExpr.implies(testExpr).isTautology()) {
        foundSpec1 = true
        countLinker += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (foundSpec1) {
      countSpec1Added += 1
      outputLine += "1,"
    } else {
      outputLine += ","
    }

    if (testNested) {
      if (testFeatures.subsetOf(nestedFeatures) && nestedExpr.implies(testExpr).isTautology()) {
        foundSpec2 = true
        countFeatureEffect += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    }

    if (testFilePc) {
      if (testFeatures.subsetOf(filePcFeatures) && filePcExpr.implies(testExpr).isTautology()) {
        foundSpec2 = true
        countFEffect_Build += 1
        outputLine += "1,"
      } else {
        outputLine += ","
      }
    } else {
      outputLine += ","
    }



    if (foundSpec2) {
      countSpec2Added += 1
      outputLine += "1,"
    } else {
      outputLine += ","
    }

    if (foundSpec1 || foundSpec2) {
      outputLine += "1,"
      countCodeAdded += 1
    } else {
      outputLine += ","
    }

    return (foundSpec1 || foundSpec2)
  }

  def resetCounts() {
    //counting variables
    countAll = 0
    countFound = 0
    countNotFound = 0
    countPreprocessor = 0
    countType = 0
    countParser = 0
    countFeatureEffect = 0
    countLinker = 0
    countFEffect_Build = 0
    countSpec1Combined = 0
    countSpec2Combined = 0
    countCodeCombined = 0
    countCodeAdded =0
    countSpec2Added = 0
  }

  def compareConstraints(fileToCompare: String, compareCombined: Boolean = false, isGroup: Boolean = false) = {
    resetCounts()

    if (!new java.io.File(fileToCompare).exists) {
      System.err.println(fileToCompare + " does not exist")
    } else {

      if (compareCombined)
        comparisonWriter.println("Constraint, #error, Parser, Type, Linker, Spec1Added, FeatureEffect, FeatureEffect_Build, Spec2Added, CodeAdded, Spec1Combined, Spec2Combined, CodeCombined")
      else
        comparisonWriter.println("Constraint, #error, Parser, Type, Linker, Spec1Added, FeatureEffect, FeatureEffect_Build, Spec2Added, CodeAdded")


      val featureExprParser = new FeatureExprParser()
      var countTaut = 0
      var countContra = 0
      for (line <- Source.fromFile(fileToCompare).getLines()) {

        val testExpr = featureExprParser.parse(line)
        var foundIndividual, foundCombined = false
        countAll += 1

        if (!testExpr.isTautology()) {

          if (!testExpr.isContradiction()) {

            outputLine = ""
            //compare individual code constraint formulas first then compare combined if selected
            foundIndividual = compareSingleExpr(line, testExpr, isGroup)

            if (compareCombined) {
              foundCombined = compareCombinedExpr(line, testExpr)
            }
          } else {
            notFoundWriter.println("CONTRADICTION: " + testExpr)
            countContra += 1
          }

        } else {
          notFoundWriter.println("TAUTOLOGY: " + testExpr)
          countTaut += 1
        }

        if (!foundIndividual && !foundCombined) {
          notFoundWriter.println("NOT FOUND: " + line)
        }

        comparisonWriter.println(outputLine)
      }


      statsWriter.println("numOfConstraints," + countAll + ",100")
      statsWriter.println("numOfTaut," + countTaut + "," + percentage(countTaut))
      statsWriter.println("numOfContr," + countContra + "," + percentage(countContra))
      statsWriter.println("preprocessor," + countPreprocessor + ", " + percentage(countPreprocessor))
      statsWriter.println("parser," + countParser + "," + percentage(countParser))
      statsWriter.println("type," + countType + "," + percentage(countType))
      statsWriter.println("linker," + countLinker + "," + percentage(countLinker))
      statsWriter.println("spec1Added," + countSpec1Added + "," + percentage(countSpec1Added))
      statsWriter.println("feffect," + countFeatureEffect + "," + percentage(countFeatureEffect))
      statsWriter.println("feffect_build," + countFEffect_Build + "," + percentage(countFEffect_Build))
      statsWriter.println("spec2Added," + countSpec2Added + "," + percentage(countSpec2Added))
      statsWriter.println("codeAdded," + countCodeAdded + "," + percentage(countCodeAdded))
      statsWriter.println("spec1Combined, " + countSpec1Combined + "," + percentage(countSpec1Combined))
      statsWriter.println("spec2Combined, " + countSpec2Combined + "," + percentage(countSpec2Combined))
      statsWriter.println("codeCombined, " + countCodeCombined + "," + percentage(countCodeCombined))


    }
  }

  def compareCombinedExpr(line: String, testExpr: FeatureExpr): Boolean = {
    val testFeatures = testExpr.collectDistinctFeatures
    var foundSpec1 = false
    var foundSpec2 = false
    var found = false

    //test for implication in combined code formulas and alter output line accordingly
    if (testFeatures.subsetOf(spec1Features) && spec1.implies(testExpr).isTautology) {
      outputLine += "1,"
      foundSpec1 = true
      found = true
      countSpec1Combined += 1
    } else {
      outputLine += ","
    }

    if (testFeatures.subsetOf(spec2Features) && spec2.implies(testExpr).isTautology) {
      outputLine += "1,"
      foundSpec2 = true
      found = true
      countSpec2Combined += 1
    } else {
      outputLine += ","
    }

    if (testFeatures.subsetOf(bigConjunctionFeatures) && bigConjunction.implies(testExpr).isTautology) {
      outputLine += "1,"
      countCodeCombined += 1
      found = true
    } else {
      outputLine += ","
    }

    found
  }

  def percentage(count: Int) = math rint ((count.asInstanceOf[Double] / countAll.asInstanceOf[Double]) * 100)
}

