package gsd.farce.featureeffect

import gsd.farce.utilities.Config
import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import java.util.Properties
import java.io.{FileWriter, PrintWriter, File}
import gsd.farce.utilities.PropertyKeys._
import sat.SATFeatureExprFactory
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import io.Source
import collection.mutable
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 11/07/13
 * Time: 5:20 PM
 * To change this template use File | Settings | File Templates.
 */
/*
args:
  - sysname
  - input feature list to use (should have blacklist removed from it)
 */
object FeatureEffectConstrExtractorCommon {

  val properties = new Properties()

  def main(args: Array[String]) {

    FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.sat)

    var fileList: List[String] = null
    var config: Config = null

    loadPropertiesFile(args(0), properties)

    config = getSystemConfig(args(0))

    val path = config.getSourceDir

    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

    fileList = filterFileList(fileList, path)

    //read the features we are interested in from a file (gives us control to remove problematic features)
    val distinctFeatures = Source.fromFile(args(1)).getLines().toSet
    System.err.println("model has " + distinctFeatures.size + " distinct features")


    val (constraints, featureEffectFormula) = computeNestingConstraints(path, fileList, distinctFeatures, config.getPrefix, config.getSuffix)

    val constraintWriter: PrintWriter = new PrintWriter(new FileWriter("testNested/commonFeaturesConstraints.txt"))

    constraints.foreach(constraint => {
      constraint.print(constraintWriter)
      constraintWriter.println()
    })

    constraintWriter.close()

    val formulaWriter: PrintWriter = new PrintWriter(new FileWriter("testNested/commonFeaturesFormula.txt"))

    featureEffectFormula.print(formulaWriter)
    formulaWriter.println()

    formulaWriter.close()
    System.err.println("printed out formula")
  }

  def getBlockPcs(path: String, fileList: List[String], vmDistinctFeatures: Set[String]): (Vector[FeatureExpr], mutable.HashMap[String, Set[Int]]) = {

    var featureMap = mutable.HashMap[String, Set[Int]]()
    var allBlockPcs = Vector[FeatureExpr]()
    var blockString = Set[String]()
    var index = 0 //index of block pc expression in Vector
    var singletonBlockPcFeatures = Set[String]()

    //read all block pcs from nested files and only process unique ones
    //For each block pc,add it to a vector, and put its index in a feature map with related features
    //Result is  vector allBlockPcs with an indexed set of unique blocks
    //and a feature map which maps a feature to the blockPcs it appears in
    fileList.foreach {
      nestedFile =>
        System.err.println("File: " + nestedFile)
        io.Source.fromFile(path + nestedFile + ".nested").getLines().foreach {
          line =>
            if (!blockString.contains(line)) {
              blockString += line
              try {
                val input = new ANTLRInputStream(line)
                val lexer = new FExprLexer(input)
                val tokens = new CommonTokenStream(lexer)
                val parser = new FExprParser(tokens)
                val blockpc = parser.fexpr().value

                val blockPcDistinctFeatures = blockpc.collectDistinctFeatures


                blockPcDistinctFeatures.foreach(feature => {
                  if (featureMap.contains(feature)) {
                    featureMap += feature -> (featureMap.get(feature).get + index)
                  } else {
                    featureMap += feature -> Set(index)
                  }
                })


                //add blockpc to vector, and increment index
                allBlockPcs :+= blockpc
                index += 1
              } catch {
                case e: Exception => {
                  System.err.println("Exception in file  " + nestedFile)
                  e.printStackTrace()
                }
              }
            }
        }
    }

    System.err.println("INFO: got " + allBlockPcs.size + " block pcs")
    System.err.println("num of singleton features " + singletonBlockPcFeatures.size)
    System.err.println(featureMap.size + " distinct features appear in nested expressions")

    (allBlockPcs, featureMap)
  }


  def computeNestingConstraints(path: String, fileList: List[String], vmDistinctFeatures: Set[String], prefix: String, suffix: String) = {
    var nestingConstraints = Set[FeatureExpr]()
    var featureEffectFormula = SATFeatureExprFactory.True

    val (allBlockPcs, featureMap) = getBlockPcs(path, fileList, vmDistinctFeatures)

    val keyMapWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(NESTED_IFDEF_KEYMAP))) // print the key map for debugging


    featureMap.keysIterator.foreach(key => keyMapWriter.println(featureMap.get(key).get.size + " : " + key))
    featureMap.keysIterator.foreach(key => keyMapWriter.println(key))

    keyMapWriter.close()


    /*
    For each feature X,find block pcs that contain these feature (if it doesnt, then unique will be false anyways)
    then for each of these pcs, get pc(X=TRUE) xor pc(X=False) through the unique function
    Once done, constraints becomes  X=> disjunction of different unique results

    we only need to loop on the features that have an effect on some expression (i.e., appeared in at least one
    block pc          */
    var countFeature = 1

    var countConstraint = 0
    for (feature <- featureMap.keySet) {
      System.err.println("----------analyzing feature: " + countFeature + " : " + feature + " which has " + featureMap.get(feature).get.size + " related expr ")

      val currFeature = FeatureExprFactory.createDefinedExternal(feature)
      var relatedExpr = Set[Set[String]]()
      var globalEffect = vmDistinctFeatures

      if (featureMap.contains(feature))
        featureMap.get(feature).get.foreach(index => {
          val currentExpr = allBlockPcs(index)
          val currFeatures = currentExpr.collectDistinctFeatures - feature

          //if the feature appears alone, then don't add it because the remaining feature set will be empty and intersection will then be empty
          if (!currFeatures.isEmpty) {
            relatedExpr += currFeatures
            globalEffect = globalEffect & currFeatures //get common features in ll expressions
          }

        })

      val constraint = (currFeature.implies(getConstraint(globalEffect)))
      nestingConstraints += constraint
      featureEffectFormula = featureEffectFormula.and(constraint)
      countFeature += 1
    }


    System.err.println("num of constraints: " + nestingConstraints.size)
    (nestingConstraints, featureEffectFormula)
  }

  def getConstraint(features: Set[String]): FeatureExpr = {
    if (features.isEmpty) {
      return FeatureExprFactory.True
    }

    var expression = FeatureExprFactory.True
    for (feature <- features) {
      expression = expression.and(FeatureExprFactory.createDefinedExternal(feature))
    }

    expression
  }

  def filterFileList(files: List[String], path: String) = {
    files.filter(file => (new File(path + file + ".nested")).exists)
  }
}
