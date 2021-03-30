import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser}
import junit.framework.TestCase
import org.junit.Test
import gsd.farce.features.FeatureUtils._

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 13/02/13
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */
class ImplicationTest extends TestCase {

  @Test
  def testImplication() {
    val fexpr = new FeatureExprParser(FeatureExprFactory.sat).parse("def(CONFIG_B) => def(CONFIG_C)").asInstanceOf[SATFeatureExpr]



    println(isImplication(fexpr, "CONFIG_B", "CONFIG_C"))
  }

}
