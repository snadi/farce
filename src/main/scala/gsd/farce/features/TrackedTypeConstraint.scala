package gsd.farce.features

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.error.Severity

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 10/10/13
 * Time: 8:51 AM
 * To change this template use File | Settings | File Templates.
 */
class TrackedTypeConstraint(file: String, constraint: FeatureExpr, severity: Severity.Severity) extends TrackedConstraint(file, constraint) {

  def getSeverity = severity

  override def equals(obj: Any) = {
     if (obj.isInstanceOf[TrackedTypeConstraint]){
       val toCompare = obj.asInstanceOf[TrackedTypeConstraint]
       toCompare.getConstraint.equals(constraint)  && toCompare.getSeverity.equals(severity)
     }else{
       false
     }
  }

  override def hashCode() = {
    var hash = 7;
    hash = 31 * hash + constraint.hashCode()
    hash = 31 * hash + severity.hashCode()
    hash
  }

  def getLabel = {
    severity match {
      case Severity.Crash => "criticalError_"
      case Severity.IdLookupError =>  "idLookupError_"
      case Severity.FieldLookupError => "fieldLookupError_"
      case Severity.TypeLookupError => "typeLookupError_"
      case Severity.RedeclarationError => "redeclError_"
      case Severity.Warning => "warning_"
      case Severity.SecurityWarning => "securityError_"
      case Severity.OtherError => "otherError_"
    }
  }
}
