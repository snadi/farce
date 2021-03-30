package gsd.farce.linker

import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, CSignature}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprParser, FeatureExprFactory}
import gsd.farce.filepcs.FilePCUtils._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}
import gsd.farce.utilities.Utilities._
import gsd.farce.utilities.{Linux_2_6_33_3, Config}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 18/01/13
 * Time: 2:29 PM
 * To change this template use File | Settings | File Templates.
 */

/*
Utils to be used for Linker classes
 */
trait LinkerUtils {

  def hasSameSig(sig1: CSignature, sig2: CSignature) = (sig1.name == sig2.name) && (sig1.ctype == sig2.ctype)

  def hasSameName(sig1: CSignature, sig2: CSignature) = (sig1.name == sig2.name)

  def getVMConstraints(config: Config): Iterator[FeatureExpr] = {
    for (l: String <- io.Source.fromFile(config.getFeatureModelFile).getLines(); if (l != ""))
    yield new FeatureExprParser(FeatureExprFactory.default).parse(l)
  }


  def getVM(config: Config): FeatureExpr = getVMConstraints(config).fold(FeatureExprFactory.True)(_ and _)

  //this version filters out imports that don't have corresponding exports
  def getImports(list: List[CInterfaceWrapper], exports: Seq[CSignatureWrapper]): Seq[CSignatureWrapper] = {
    var imports: Seq[CSignatureWrapper] = getImports(list)

    System.err.println("size before: " + imports.size)
    //only use imports that have corresponding exports
    imports = imports.filter(f => exports.contains(f))
    //imports = imports.filter(f => exports.find(f).get!= null && (!exports.find(f).get.fexpr.and(f.fexpr).isContradiction()))
    System.err.println("size after: " + imports.size)


    imports
  }


  def getImports(list: List[CInterfaceWrapper]): Seq[CSignatureWrapper] = {
    var imports: Seq[CSignatureWrapper] = Nil
    for (interface <- list) {
      imports ++= interface.imports.map(f => new CSignatureWrapper(interface.getFile, new CSignature(f.name, f.ctype, f.fexpr.and(getFilePC(interface.getFile)), f.pos)))
    }

    imports
  }

  def getExports(list: List[CInterfaceWrapper]): Seq[CSignatureWrapper] = {
    var exports: Seq[CSignatureWrapper] = Nil
    for (interface <- list) {
      exports ++= interface.exports.filter(i => !i.fexpr.isContradiction()).map(f => new CSignatureWrapper(interface.getFile, new CSignature(f.name, f.ctype, f.fexpr.and(getFilePC(interface.getFile)), f.pos)))
    }


    exports
  }

  def findRelatedExports(imported: CSignatureWrapper, exports: Seq[CSignatureWrapper]): FeatureExpr = {

    val featureExprParser = new FeatureExprParser()

    //related exports are those that have the same signature as the import
    val relatedExports = exports.filter(export => hasSameSig(imported, export))

    if (!relatedExports.isEmpty) {
      //joining relatedExports by oneOf (only one definition can be present at a time)
      val exportExpr = featureExprParser.oneOf(relatedExports.map(x => x.fexpr).toSet.toList)

      exportExpr
    } else {
      null
    }

  }

}


object LinuxHelp extends LinkerUtils {
  val config: Config = Linux_2_6_33_3

  def getLinuxVMConstraints: Iterator[FeatureExpr] =
    for (l: String <- io.Source.fromFile(config.getFeatureModelFile).getLines(); if (l != ""))
    yield new FeatureExprParser(FeatureExprFactory.default).parse(l)


  def getLinuxVM(): FeatureExpr = getLinuxVMConstraints.fold(FeatureExprFactory.True)(_ and _)

  val path = config.getSourceDir
  val filesfile = config.getFileListFile
  // val featuresfile = path + "features"

  var fileList = io.Source.fromFile(filesfile).getLines().toList
  // val featureList = io.Source.fromFile(featuresfile).getLines().toList
}
