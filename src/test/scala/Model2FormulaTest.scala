import gsd.farce.features.model.FeatureConstraintTypesMain
import gsd.linux.KConfigParser
import java.io.File
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 12.07.13
 * Time: 20:37
 * To change this template use File | Settings | File Templates.
 */
class Model2FormulaTest extends FunSuite{

  val uclibc = KConfigParser.parseKConfigFile( this.getClass.getResource("uclibc/uClibc-0.9.33.2.exconfig").getPath )

  test( "partial configuration" ){
    val toTrue = Set( "TARGET_i386")
    val toFalse = Set("TARGET_arm", "TARGET_avr32")
    val (cnf,generated,idMap) = FeatureConstraintTypesMain.kconfig2CNFfull( uclibc, toTrue, toFalse )
    for( i <- toTrue++toFalse )
      assert( !( idMap contains i ) )


  }

}
