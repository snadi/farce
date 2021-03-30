package gsd.farce.features

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.error.Severity._
import de.fosd.typechef.error.Severity
import de.fosd.typechef.error.Severity.Severity

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 10/10/13
 * Time: 8:51 AM
 * To change this template use File | Settings | File Templates.
 */
class TrackedConstraint(file: String, constraint: FeatureExpr) {

  def getFile = file
  def getConstraint = constraint

  override def equals(obj: Any) = {
     if (obj.isInstanceOf[TrackedConstraint]){
       val toCompare = obj.asInstanceOf[TrackedConstraint]
       toCompare.getConstraint.equals(constraint)
     }else{
       false
     }
  }

  override def hashCode() = {
    var hash = 7;
    hash = 31 * hash + constraint.hashCode()
    hash
  }
}
