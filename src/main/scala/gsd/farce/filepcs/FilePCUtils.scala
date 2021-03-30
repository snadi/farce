package gsd.farce.filepcs

import java.io.File
import io.Source
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, FeatureExprParser}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 11/02/13
 * Time: 1:03 PM
 * To change this template use File | Settings | File Templates.
 */
object FilePCUtils {

  def getFilePC(file: String): FeatureExpr = {
    var pcCondition = FeatureExprFactory.True

    val fileName = getBaseName(file) + ".pc"

    //get file pc from .pc file. If file doesn't exist, then pc is TRUE
    if (new File(fileName).exists()) {
      val source = Source.fromFile(fileName)
      for (line <- source.getLines()) {
        pcCondition = new FeatureExprParser(FeatureExprFactory.default).parse(line)
      }

      source.close()
    }

    pcCondition
  }

  def getBaseName(fileName: String): String = {

    if (fileName.endsWith(".c"))
      fileName.substring(0, fileName.indexOf(".c"))
    else if (fileName.endsWith(".h"))
      fileName.substring(0, fileName.indexOf(".h"))
    else if (fileName.endsWith(".S"))
      fileName.substring(0, fileName.indexOf(".S"))
    else
      fileName
  }

}
