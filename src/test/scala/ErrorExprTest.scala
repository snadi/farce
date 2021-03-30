import de.fosd.typechef.featureexpr.FeatureExprFactory
import java.io.FileWriter
import junit.framework.TestCase
import org.junit.{Assert, Test}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 13/02/13
 * Time: 8:37 AM
 * To change this template use File | Settings | File Templates.
 */
class ErrorExprTest extends TestCase {

  @Test
  def testErrorConc() {
    val file1 = getClass().getResource("file1.c.xml")

    val fileList = List("file1", "file2")

    val errorExpr = FeatureExprFactory.True// getErrorExpr(fileList, file1.getPath().substring(0, file1.getPath().indexOf("file1.c.xml")))

    println(errorExpr)

    //write out the formula file so we can use it in the ModelComparerTest
    val out = new FileWriter(file1.getPath().substring(0, file1.getPath().indexOf("file1.c.xml")) + "errorsFormula.txt")
    println(file1.getPath().substring(0, file1.getPath().indexOf("file1.c.xml")) + "errorsFormula.txt")
    out.write(errorExpr.toString())
    out.write("\n")
    out.close()

  }


}
