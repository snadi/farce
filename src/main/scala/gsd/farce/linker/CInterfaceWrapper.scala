package gsd.farce.linker

import de.fosd.typechef.typesystem.linker.{CSignature, CInterface}
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 19/02/13
 * Time: 1:53 PM
 * To change this template use File | Settings | File Templates.
 */
class CInterfaceWrapper(file: String, cInterface: CInterface) extends CInterface(cInterface.featureModel, cInterface.importedFeatures, cInterface.declaredFeatures, cInterface.imports, cInterface.exports) {

  def getFile = file
}
