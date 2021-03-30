import gsd.farce.features.model.BExprUtils
import gsd.farce.features.model.translation.{FarceBooleanTranslation, FarceTristateTranslation}
import gsd.linux.{BTrue, BFalse, TristateTranslation, KConfigParser}
import org.scalatest.FunSuite

/**
 * Created by berger on 20/02/14.
 */
class KconfigTranslationTest extends FunSuite {

  test("check tristate translation (multiple.Kconfig"){

    val k = KConfigParser.parseKConfigFile( this.getClass.getResource("multiple.pb").getPath )
    val tt = new TristateTranslation( k )
    val oldF = tt.translate.toSet

    val tt2 = new FarceTristateTranslation( k )
    val newF = tt2.translate.toSet

    println("old size: " + oldF.size )
    println("new size: " + newF.size )
    oldF.filterNot( newF contains _ ) foreach println

  }

  test("check tristate translation (multiple-2.Kconfig"){

      val k = KConfigParser.parseKConfigFile( this.getClass.getResource("multiple-2.pb").getPath )
      val tt = new TristateTranslation( k )
      val oldF = tt.translate.toSet

      val tt2 = new FarceTristateTranslation( k )
      val newF = tt2.translate.toSet

      println("old size: " + oldF.size )
      println("new size: " + newF.size )
      oldF.filterNot( newF contains _ ) foreach println

  }

  test("run tristate translation (multiple-2.Kconfig"){

    val k = KConfigParser.parseKConfigFile( this.getClass.getResource("multiple-2.pb").getPath )
    val tt2 = new FarceTristateTranslation( k )
    tt2.translate foreach println
  }

  test("run boolean translation (multiple-2.kconfig"){
    val k = KConfigParser.parseKConfigFile( this.getClass.getResource("multiple-2.pb").getPath )
    val trans = FarceBooleanTranslation.mkBooleanTranslation( k )
    println( trans.genVars )
//    BExprUtils.simplify( trans.exprs) foreach println
    trans.exprs foreach println
  }

  test("run boolean translation (Kconfig"){
      val k = KConfigParser.parseKConfigFile( this.getClass.getResource("Kconfig.pb").getPath )
      val trans = FarceBooleanTranslation.mkBooleanTranslation( k )
      println( trans.genVars )
      BExprUtils.simplify( trans.exprs) foreach println
//      trans.exprs foreach println
    }

  test("run boolean translation (uclibc"){
      val k = KConfigParser.parseKConfigFile( this.getClass.getResource("uclibc/uClibc-0.9.33.2.exconfig").getPath )
      val trans = FarceBooleanTranslation.mkBooleanTranslation( k )
      println( trans.genVars )
//      trans.exprs foreach println
      BExprUtils.simplifyAggressive(trans.exprs) foreach println
//      println( BExprUtils.simplifyAggressive( trans.exprs) )
    }

}
