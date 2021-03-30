package gsd.farce.features

import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by snadi on 25/12/13.
 */
class TrackedLinkerConstraint(file: String, constraint: FeatureExpr, symbolName: String, constraintType: String) extends TrackedConstraint(file, constraint){

  def getSymbol = symbolName
  def getConstraintType = constraintType

  override def equals(obj: Any) = {
    if (obj.isInstanceOf[TrackedLinkerConstraint]){
      val toCompare = obj.asInstanceOf[TrackedLinkerConstraint]
      toCompare.getConstraint.equals(constraint)//  && toCompare.getSymbol.equals(symbolName)
    }else{
      false
    }
  }

  override def hashCode() = {
    var hash = 7;
    hash = 31 * hash + constraint.hashCode()
  //  hash = 31 * hash + symbolName.hashCode()
    hash
  }

}
