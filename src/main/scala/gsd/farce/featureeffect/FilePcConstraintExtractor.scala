package gsd.farce.featureeffect


import de.fosd.typechef.featureexpr._
import antlr.{FExprParser, FExprLexer}
import java.util.Properties
import java.io.{FileWriter, PrintWriter, File}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import io.Source
import collection.mutable
import gsd.farce.utilities.PropertyKeys._
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.Config

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 11/07/13
 * Time: 5:20 PM
 * To change this template use File | Settings | File Templates.
 */
/*
extracts the feature effect constraints from the build system
args(0): name of system
args(1): file with feature names we are interested in
 */
object FilePcConstraintExtractor {


  def main(args: Array[String]) {

    FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.sat)

    var fileList: List[String] = null
    var config: Config = null

    val properties = new Properties()

    loadPropertiesFile(args(0), properties)

    config = getSystemConfig(args(0))

    val path = config.getSourceDir
    fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

    fileList = filterFileList(fileList, path)

    //read the features we are interested in from a file (gives us control to remove problematic features)
    val distinctFeatures = Source.fromFile(args(1)).getLines().toSet
    System.err.println("analyzing " + distinctFeatures.size + " distinct features")

    val filePcFormWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(FILE_PC_FORMULA_FILE)))

    val filePcConstraints = computeFilePcConstraints(path, fileList, distinctFeatures, config.getPrefix, config.getSuffix)

    val filePcFormula = if(!filePcConstraints.isEmpty) filePcConstraints.reduceLeft(_ and _)  else null

    val filePcConstWriter: PrintWriter = new PrintWriter(new FileWriter(properties.getProperty(FILE_PC_CONSTRAINTS_FILE)))

    if (filePcFormula != null)
      filePcFormula.print(filePcFormWriter)

    filePcFormWriter.close()

    filePcConstraints.map(x => {
      x.print(filePcConstWriter)
      filePcConstWriter.println()
    })

    filePcConstWriter.close()


  }

  def getFilePcs(path: String, fileList: List[String], vmDistinctFeatures: Set[String]): (Vector[FeatureExpr], mutable.HashMap[String, Set[Int]]) = {

    var featureMap = mutable.HashMap[String, Set[Int]]()
    var allFilePcs = Vector[FeatureExpr]()
    var pcStringSet = Set[String]()
    var index = 0 //index of block pc expression in Vector
    var singletonBlockPcFeatures = Set[String]()

    //read all block pcs from pc files and only process unique ones
    //For each block pc,add it to a vector, and put its index in a feature map with related features
    //Result is  vector allBlockPcs with an indexed set of unique blocks
    //and a feature map which maps a feature to the blockPcs it appears in
    fileList.foreach {
      filepc =>
        System.err.println("File: " + filepc)
        val fileReader = Source.fromFile(path + filepc + ".pc")

        fileReader.getLines().foreach {
          line =>
            if (!pcStringSet.contains(line)) {
              pcStringSet += line
              try{
              val input = new ANTLRInputStream(line)
              val lexer = new FExprLexer(input)
              val tokens = new CommonTokenStream(lexer)
              val parser = new FExprParser(tokens)
              val blockpc = parser.fexpr().value

              val blockPcDistinctFeatures = blockpc.collectDistinctFeatures

              //Optimization: if the block pc has one feature, then we don't need to keep track of this block pc
              //because only one feature will have an effect on it, and which we are already removing
              //from our list of analyzed features. This reduces the number of block pcs to loop on later on
              if (blockPcDistinctFeatures.size != 1) {

                blockPcDistinctFeatures.foreach(feature => {

                  //if this feature already appeared in a singleton block pc, then no need to check for it
                  //since its constraint will be f = > True
                  if (!singletonBlockPcFeatures.contains(feature) && featureMap.contains(feature)) {
                    featureMap += feature -> (featureMap.get(feature).get + index)
                  } else {
                    featureMap += feature -> Set(index)
                  }
                })


                //add blockpc to vector, and increment index
                allFilePcs :+= blockpc
                index += 1
              } else {
                val feature = blockPcDistinctFeatures.head
                singletonBlockPcFeatures += feature
              }
            }catch{
                case e:Exception => {
                  System.err.println("Exception in file  " + filepc)
                e.printStackTrace()}
              }
            }
        }

        fileReader.close()
    }

    System.err.println("BEFORE: " + featureMap.keySet.size)
    singletonBlockPcFeatures.foreach(f => featureMap.remove(f))
    System.err.println("AFTER removing singletong: " + featureMap.keySet.size)
    featureMap.filter(x => !vmDistinctFeatures.contains(x._1))

    System.err.println("AFTER removing non-open list: " + featureMap.keySet.size)

    val blackList = Source.fromFile("output/FilePcs/featureBlacklist").getLines().map(_.trim).toSet

    blackList.foreach(f => featureMap.remove(f))

    System.err.println("INFO: got " + allFilePcs.size + " file pcs")
    System.err.println("num of singleton features " + singletonBlockPcFeatures.size)
    System.err.println(featureMap.size + " distinct features appear in file pc expressions")

    (allFilePcs, featureMap)
  }

  def computeFilePcConstraints(path: String, fileList: List[String], vmDistinctFeatures: Set[String], prefix: String, suffix: String) = {
    var filePcConstraints = Set[FeatureExpr]()

    val (allFilePcs, featureMap) = getFilePcs(path, fileList, vmDistinctFeatures)

    val keyMapWriter: PrintWriter = new PrintWriter(new FileWriter("output/FilePcs/keyMap"))


    featureMap.keysIterator.foreach(key => keyMapWriter.println(featureMap.get(key).get.size + " : " + key ) )
    featureMap.keysIterator.foreach(key => keyMapWriter.println(key ) )

    keyMapWriter.close()


    /*
    For each feature X,find file pcs that contain these feature (if it doesnt, thn unique will be false anyways)
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
              val effect = allFilePcs(index).unique(currFeature)

               //for debugging specific feature
             /* if(feature == "__UCLIBC_HAS_CRYPT_IMPL__"){
                System.err.println("expr: " + allFilePcs(index))
                System.err.println("effect: " + effect)
              }*/

              globalEffect = globalEffect.or(effect)
          })

        try {
            System.err.println("Finished disjunction of all related file pcs and distinct features: " + globalEffect.collectDistinctFeatures.size)
            val constraint = currFeature.implies(globalEffect)
            countConstraint += 1

            //for debugging specific feature
           /* if(feature == "__UCLIBC_HAS_CRYPT_IMPL__"){
              System.err.println("global effect: " + globalEffect)
            }*/
            System.err.println("created constraint")



            filePcConstraints += constraint

        } catch {
          case e: Exception => {
            System.err.println("Exception for feature: " + feature + " with expression size: "+ globalEffect.size)
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


    System.err.println("num of constraints: " + filePcConstraints.size)
    filePcConstraints
  }

  def filterFileList(files: List[String], path: String) = {
    files.filter(file => (new File(path + file + ".pc")).exists)
  }
}
