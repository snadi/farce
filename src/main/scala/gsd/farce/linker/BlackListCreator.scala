package gsd.farce.linker

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel, FeatureExpr}
import java.io.{File, FileWriter, PrintWriter}
import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, SystemLinker, InterfaceWriter}
import gsd.farce.utilities.Utilities._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 26/07/13
 * Time: 9:43 AM
 * To change this template use File | Settings | File Templates.
 */

//Copied from TypeChef-BusyboxAnalysis Linker code
//creates a blacklist of functions that we should not use (whose conditions are conflicting etc.)
//takes in the system name and the output blacklist file
object BlackListCreator extends App with LinkerUtils{

  var featureModel: FeatureExpr = null
  var fileList: List[String] = null
  val config = getSystemConfig(args(0))
  var vm: FeatureModel = null
  var blackList = Set[String]()

  if (!config.getFeatureModelFile.endsWith(".dimacs")) {
    featureModel = getVM(config)
    vm = FeatureExprFactory.default.featureModelFactory.create(featureModel)
  } else {
    vm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFile(config.getFeatureModelFile)
  }

  fileList = io.Source.fromFile(config.getFileListFile).getLines().toList

  val blackListWriter: PrintWriter = new PrintWriter(new FileWriter(args(1)))


  println("parsing")

  val reader = new InterfaceWriter() {}

  println(fileList.size + " files")

  def filterFileList(files: List[String]) = {
    files.filter(file => new File(config.getSourceDir + file + config.getInterfaceExtension).exists())

  }

  fileList = filterFileList(fileList)

  var countLinking = 0
  var interfaces = fileList.map(f =>{
    println("reading file:" + f)
    reader.readInterface(new File(config.getSourceDir + f + config.getInterfaceExtension))
  }).map({  interface =>
    println("linking: " + countLinking)
  countLinking += 1
    SystemLinker.linkStdLib(interface)
  })

  println("composing")

  var finalInterface: CInterface = EmptyInterface

  val t1 = System.currentTimeMillis()

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


  finalInterface = linkTreewise(interfaces)

  val t2 = System.currentTimeMillis()

  println("total composition time: " + (t2 - t1))

  finalInterface = finalInterface.andFM(featureModel).pack

  finalInterface.imports.foreach(f => blackList += f.name)

  blackList.foreach(blackListWriter.println(_))

  blackListWriter.close()
}
