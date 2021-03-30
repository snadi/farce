package constraints

import org.scalatest.FunSuite
import de.fosd.typechef.featureexpr.FeatureExprFactory
import gsd.farce.features.{DimacsReader, CreateDimacs}
import de.fosd.typechef.featureexpr.sat.SATFeatureExpr

/**
 * Created by snadi on 02/01/14.
 */
class DimacsReaderTest extends FunSuite{

  test( "A and B" ){
    val testExpr = FeatureExprFactory.createDefinedExternal("A").and(FeatureExprFactory.createDefinedExternal("B"))

    CreateDimacs.createDimacs(testExpr.asInstanceOf[SATFeatureExpr],"src/test/resources/dimacsReader1.dimacs", false,"","")

    val dimacsReader = new DimacsReader()
    dimacsReader.readDimacs("src/test/resources/dimacsReader1.dimacs")
    assert(dimacsReader.getFeatureExpr.equals(testExpr))

  }

  test( "A or B" ){
    val testExpr = FeatureExprFactory.createDefinedExternal("A").or(FeatureExprFactory.createDefinedExternal("B"))

    CreateDimacs.createDimacs(testExpr.asInstanceOf[SATFeatureExpr],"src/test/resources/dimacsReader2.dimacs", false,"","")

    val dimacsReader = new DimacsReader()
    dimacsReader.readDimacs("src/test/resources/dimacsReader2.dimacs")
    assert(dimacsReader.getFeatureExpr.equals(testExpr))
  }

  test( "A and (B || C)" ){
    val testExpr = FeatureExprFactory.createDefinedExternal("A").and(FeatureExprFactory.createDefinedExternal("B").or(FeatureExprFactory.createDefinedExternal("C")))

    CreateDimacs.createDimacs(testExpr.asInstanceOf[SATFeatureExpr],"src/test/resources/dimacsReader3.dimacs", false,"","")

    val dimacsReader = new DimacsReader()
    dimacsReader.readDimacs("src/test/resources/dimacsReader3.dimacs")
    assert(dimacsReader.getFeatureExpr.equals(testExpr))
  }
}
