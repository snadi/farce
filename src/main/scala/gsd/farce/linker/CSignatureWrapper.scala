package gsd.farce.linker

import de.fosd.typechef.typesystem.linker.CSignature

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 19/02/13
 * Time: 2:15 PM
 * To change this template use File | Settings | File Templates.
 */
class CSignatureWrapper(file: String, cSig: CSignature) extends CSignature(cSig.name, cSig.ctype, cSig.fexpr, cSig.pos) {

  var alreadyAnalyzed = false

  def getFile = file


  override def hashCode = name.hashCode + ctype.hashCode()

  override def equals(that: Any) = that match {
    case CSignature(thatName, thatCType, thatFexpr, thatPos, set) => name == thatName && ctype == thatCType
    case _ => false
  }

  //tmp for debugging
  override def toString = "---" + getFile + ":" + name + ": " + ctype.toText + ":" + fexpr + "----"

}
