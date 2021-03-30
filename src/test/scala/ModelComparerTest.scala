import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import gsd.farce.features.FeatureUtils._
import java.io.File
import junit.framework.TestCase
import org.junit.{Assert, Test}
import gsd.farce.features._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 13/02/13
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */
class ModelComparerTest extends TestCase {

  @Test
  def testModelComparer() {

    val fileName = getClass().getResource("gitbusybox/featureModel").toString

    val featureModelFile = new File(fileName)

    val fexpr = new FeatureExprParser(FeatureExprFactory.sat).parse("def(CONFIG_C) => def(CONFIG_B)").asInstanceOf[SATFeatureExpr]

    println("building 1")
    val featureModelImplGraph = buildImplGraph(fexpr, "feature model")


    val featureExprParser = new FeatureExprParser(FeatureExprFactory.default)
    val errorExpr = featureExprParser.parse("((!def(CONFIG_D)|(def(CONFIG_A)&def(CONFIG_C)))&(def(CONFIG_A)|def(CONFIG_B)|!def(CONFIG_C)))")

    println("building 2")
    val erroModelGraph = buildImplGraph(errorExpr, "error model")

    val interfaceExpr = featureExprParser.parse("def(CONFIG_C) => def(CONFIG_B)")

    println("building 3")
    val interfaceGraph = buildImplGraph(interfaceExpr, "interface model")




    //modelComparer.compareImplications()
  }

}
