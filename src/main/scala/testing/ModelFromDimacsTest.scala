package testing

import de.fosd.typechef.featureexpr.sat.{SATFeatureExpr, SATFeatureModel}
import java.io.File
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import gsd.farce.features.CreateDimacs


/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 04/09/13
 * Time: 7:48 AM
 * To change this template use File | Settings | File Templates.
 */
object ModelFromDimacsTest extends App{


  val typeModel = SATFeatureModel.createFromDimacsFile(args(0), "", "")


  println("read type model")


    //typeModel.writeToDimacsFile(new File("output/TypeErrors/rewrite.dimacs"))

  val featureExprParser = new FeatureExprParser()
    val test1 = featureExprParser.parse("CONFIG_SYSVIPC_SYSCTL => CONFIG_SYSVIPC")
  val test2 = featureExprParser.parse("!CONFIG_B&&CONFIG_A")
  val test3 = featureExprParser.parse("CONFIG_A&&CONFIG_C")



  println("new expr formula satisfiable: " + test1.isTautology(typeModel))



 /* println(test1.toString() + " can be satisfied: " + test1.isSatisfiable(typeModel))

  println(test2.toString() + " can be satisfied: " + test2.isSatisfiable(typeModel))
  println(test3.toString() + " can be satisfied: " + test3.isSatisfiable(typeModel))*/

}
