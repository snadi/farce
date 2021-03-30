package gsd.farce.featureeffect

import gsd.farce.utilities.Config
import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import java.util.Properties
import java.io.{FileWriter, PrintWriter, File}
import gsd.farce.utilities.PropertyKeys._
import sat.{SATFeatureExprFactory}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import io.Source
import collection.mutable
import gsd.farce.utilities.Utilities._
import gsd.farce.filepcs.FilePCUtils._

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
object FeatureEffectConstrExtractor {

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

    val addFilePc = if (args.length > 2 && args(2).equals("true")) true else false
    val ignoreSingleton = properties.getProperty("ignoreSingleton").toBoolean
    System.err.println("ignoring singletong: " + ignoreSingleton)

    val (constraints, featureEffectFormula) = computeNestingConstraints(ignoreSingleton, addFilePc, path, fileList, distinctFeatures, config.getPrefix, config.getSuffix)

    val constraintWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(NESTED_IFDEF_CONSTRAINTS_FILE)))

    constraints.foreach(constraint => {
      constraint.print(constraintWriter)
      constraintWriter.println()
    })

    constraintWriter.close()

    val formulaWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(NESTED_IFDEF_FORMULA_FILE)))

    featureEffectFormula.print(formulaWriter)
    formulaWriter.println()

    formulaWriter.close()
    System.err.println("printed out formula")
  }

  def getBlockPcs(ignoreSingleton: Boolean, path: String, fileList: List[String], vmDistinctFeatures: Set[String]): (Vector[FeatureExpr], mutable.HashMap[String, Set[Int]]) = {

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
                val blockpc = parseConstraint(line)

                val blockPcDistinctFeatures = blockpc.collectDistinctFeatures

                //uncomment if you want to debug a specific feature
//                if (blockPcDistinctFeatures.contains("CONFIG_FEATURE_CPIO_P")){
//                  System.err.println("BLOCK PC IN FILE " + nestedFile + " has it")
//                }

                //Optimization: if the block pc has one feature, then we don't need to keep track of this block pc
                //because only one feature will have an effect on it, and which we are already removing
                //from our list of analyzed features. This reduces the number of block pcs to loop on later on
                if (blockPcDistinctFeatures.size != 1) {

                  blockPcDistinctFeatures.foreach(feature => {

                    //if this feature already appeared in a singleton block pc, then no need to check for it
                    //since its constraint will be f = > True
                    if (ignoreSingleton || (!ignoreSingleton && !singletonBlockPcFeatures.contains(feature))) {

//                      if (blockPcDistinctFeatures.contains("CONFIG_FEATURE_CPIO_P")){
//                        System.err.println("BLOCK PC IN FILE " + nestedFile + " has it")
//                      }

                      if (featureMap.contains(feature)) {
                        featureMap += feature -> (featureMap.get(feature).get + index)
                      } else {
                        featureMap += feature -> Set(index)
                      }
                    }
                  })


                  //add blockpc to vector, and increment index
                  allBlockPcs :+= blockpc
                  index += 1
                } else {
                  if (!ignoreSingleton) {
                    val feature = blockPcDistinctFeatures.head
                    singletonBlockPcFeatures += feature
                  }
                }
              } catch {
                case e: Exception => {
                  System.err.println("Exception in file  " + nestedFile)
                  e.printStackTrace()
                }
              }
            }
        }
    }

    System.err.println("BEFORE: " + featureMap.keySet.size)

    if (!ignoreSingleton) {
      //remove singleton features since their global effect will always be TRUE
      //saves us expensive computation for the rest of the block pcs
      singletonBlockPcFeatures.foreach(f => {
        System.err.println("REMOVING: " + f)
        featureMap.remove(f)
      })

      System.err.println("num of singleton features " + singletonBlockPcFeatures.size)
      singletonBlockPcFeatures.foreach(System.err.println(_))

      System.err.println("AFTER removing singleton: " + featureMap.keySet.size)
    }


    featureMap.filter(x => !vmDistinctFeatures.contains(x._1))
    System.err.println("AFTER removing non-open list: " + featureMap.keySet.size)


    System.err.println("INFO: got " + allBlockPcs.size + " block pcs")
    System.err.println(featureMap.size + " distinct features appear in nested expressions")

    (allBlockPcs, featureMap)
  }

  def getBlockPcsNoPC(ignoreSingleton: Boolean, path: String, fileList: List[String], vmDistinctFeatures: Set[String]): (Vector[FeatureExpr], mutable.HashMap[String, Set[Int]]) = {

    var featureMap = mutable.HashMap[String, Set[Int]]()
    var allBlockPcs = Vector[FeatureExpr]()
    var seenBLocks = Set[FeatureExpr]()
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
            val blockPcOnly = parseConstraint(line)
            val filePC = getFilePC(path + nestedFile)
            System.out.println("file pc :" + filePC)
            val fullBlockPc = blockPcOnly.and(filePC)

            if (!seenBLocks.contains(fullBlockPc)) {
              seenBLocks += fullBlockPc
              try {
                val blockPcOnlyDistinctFeatures = blockPcOnly.collectDistinctFeatures //we will only loop on the features that appear in the code rather than the file pc
                val fullBlockPCFeatures = fullBlockPc.collectDistinctFeatures

                //uncomment if you want to debug a specific feature
//                if (blockPcOnlyDistinctFeatures.contains("CONFIG_CPIO_P")){
//                  System.err.println("BLOCK PC IN FILE " + nestedFile + " has it")
//                }

                //Optimization: if the block pc has one feature, then we don't need to keep track of this block pc
                //because only one feature will have an effect on it, and which we are already removing
                //from our list of analyzed features. This reduces the number of block pcs to loop on later on
                if (fullBlockPCFeatures.size != 1) {

                  blockPcOnlyDistinctFeatures.foreach(feature => {

                    //if this feature already appeared in a singleton block pc, then no need to check for it
                    //since its constraint will be f = > True
                   if (!ignoreSingleton || (ignoreSingleton && !singletonBlockPcFeatures.contains(feature))) {
                    if (featureMap.contains(feature)) {
                      featureMap += feature -> (featureMap.get(feature).get + index)
                    } else {
                      featureMap += feature -> Set(index)
                    }
                   }
                  })


                  //add blockpc to vector, and increment index
                  allBlockPcs :+= fullBlockPc
                  index += 1
                } else {
                  if (!ignoreSingleton) {
                    val feature = fullBlockPCFeatures.head
                    singletonBlockPcFeatures += feature
                  }
                }
              } catch {
                case e: Exception => {
                  System.err.println("Exception in file  " + nestedFile)
                  e.printStackTrace()
                }
              }
            }
        }
    }
    System.err.println("BEFORE: " + featureMap.keySet.size)

    if (!ignoreSingleton) {
      //remove singleton features since their global effect will always be TRUE
      //saves us expensive computation for the rest of the block pcs
      singletonBlockPcFeatures.foreach(f => {
        System.err.println("REMOVING: " + f)
        featureMap.remove(f)
      })

      System.err.println("num of singleton features " + singletonBlockPcFeatures.size)
      singletonBlockPcFeatures.foreach(System.err.println(_))

      System.err.println("AFTER removing singleton: " + featureMap.keySet.size)
    }


    featureMap.filter(x => !vmDistinctFeatures.contains(x._1))
    System.err.println("AFTER removing non-open list: " + featureMap.keySet.size)


    System.err.println("INFO: got " + allBlockPcs.size + " block pcs")
    System.err.println(featureMap.size + " distinct features appear in nested expressions")

    (allBlockPcs, featureMap)
  }

  def computeNestingConstraints(ignoreSingleton: Boolean, addFilePc: Boolean, path: String, fileList: List[String], vmDistinctFeatures: Set[String], prefix: String, suffix: String) = {
    var nestingConstraints = Set[FeatureExpr]()
    var featureEffectFormula = SATFeatureExprFactory.True


    //if the .nested files contain the file pc, we will not add it separately
    val (allBlockPcs, featureMap) = if (addFilePc)
      getBlockPcsNoPC(ignoreSingleton, path, fileList, vmDistinctFeatures)
    else
      getBlockPcs(ignoreSingleton, path, fileList, vmDistinctFeatures)


    val keyMapWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(NESTED_IFDEF_KEYMAP))) // print the key map for debugging


    featureMap.keysIterator.foreach(key => keyMapWriter.println(featureMap.get(key).get.size + " : " + key))
    featureMap.keysIterator.foreach(key => keyMapWriter.println(key))

    keyMapWriter.close()


    /*
    For each feature X,find block pcs that contain these feature (if it doesnt, thn unique will be false anyways)
    then for each of these pcs, get pc(X=TRUE) xor pc(X=False) through the unique function
    Once done, constraints becomes  X=> disjunction of different unique results

    we only need to loop on the features that have an effect on some expression (i.e., appeared in at least one
    block pc          */
    var countFeature = 1

    var countConstraint = 0
    for (feature <- featureMap.keySet) {
      System.err.println("----------analyzing feature: " + countFeature + " : " + feature + " which has " + featureMap.get(feature).get.size + " related expr ")
      try {
        val currFeature = FeatureExprFactory.createDefinedExternal(feature)

        var globalEffect = FeatureExprFactory.False
        if (featureMap.contains(feature))
          featureMap.get(feature).get.foreach(index => {
            val effect = allBlockPcs(index).unique(currFeature)


            //for debugging a specific feature
             /*if(feature == "CONFIG_CPIO_P"){
              System.err.println("expr: " + allBlockPcs(index))
              System.err.println("effect: " + effect)
            }*/

            globalEffect = globalEffect.or(effect)
          })

        try {
          System.err.println("Finished disjunction of all related blocks and distinct features: " + globalEffect.collectDistinctFeatures.size)

          //for debugging a specific feature
          /*if (feature == "CONFIG_CPIO_P") {
            System.err.println("global effect of config_lzma: " + globalEffect)
          }*/

          val constraint = currFeature.implies(globalEffect)
          countConstraint += 1

          //System.err.println("created constraint")

          val newFormula = featureEffectFormula.and(constraint)

          if (newFormula.isSatisfiable()) {
            nestingConstraints += constraint
            featureEffectFormula = newFormula
          } else {
            System.err.println("CONTRADICTION from feature: " + feature + ", constraint : " + constraint)
          }


        } catch {
          case e: Exception => {
            System.err.println("Exception for feature: " + feature + " with expression size: " + globalEffect.size)
            e.printStackTrace()
          }
        }
      } catch {
        case e: Exception => {
          System.err.println("Exception for feature: " + feature)
          e.printStackTrace()
        }
      }
      countFeature += 1
    }


    System.err.println("num of constraints: " + nestingConstraints.size)
    (nestingConstraints, featureEffectFormula)
  }

  def filterFileList(files: List[String], path: String) = {
    files.filter(file => (new File(path + file + ".nested")).exists)
  }
}
