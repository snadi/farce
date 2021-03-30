package gsd.farce.features.model

import gsd.linux.KConfigParser
import gsd.linux.cnf.{ImplBuilder, SATBuilder, DimacsReader}
import scala.util.logging.ConsoleLogger

/**
 * some helper methods to query models within the REPL
 * Created by berger on 13.12.13.
 */
object ModelQueryConsole {

  lazy val multiple2 = new ModelQuery( "input/kconfig-test-models/multiple-2.dimacs", "input/kconfig-test-models/multiple-2.pb" )
  lazy val busybox = new ModelQuery( "../variability-models/kconfig/abstraction/busybox-1.21.0.dimacs", "../variability-models/kconfig/busybox-1.21.0.exconfig" )
  lazy val busyboxFeatureRelationships = FeatureRelationships.getFeatureRelationshipFinder( "../variability-models/kconfig/busybox-1.21.0.exconfig" )

  def main( args: Array[String] ){

  }


  def getSatBuilderForDimacs( f: String ) = {
    val dh = DimacsReader.readHeaderFile( f )
    val dp = DimacsReader.readFile( f )

     new SATBuilder(dp.cnf, dp.numVars, dh.generated, dh.firstGen)
          with ImplBuilder with ConsoleLogger
  }

  def test{
//    val f = "../variability-models/kconfig/abstraction/uclibc.dimacs"
//    val f = "../variability-models/kconfig/abstraction/uclibc_tristate.dimacs"
    val f = "uClibc-0.9.33.2__crosstree.dimacs"
//    val f = "uClibc-0.9.33.2__full.dimacs"
    val dh = DimacsReader.readHeaderFile( f )
    val dp = DimacsReader.readFile( f )

    val sat = new SATBuilder(dp.cnf, dp.numVars, dh.generated, dh.firstGen)
                    with ImplBuilder with ConsoleLogger

    println( sat.implication( dh.idMap.get("UCLIBC_HAS_LOCALE").get, dh.idMap.get("UCLIBC_HAS_WCHAR").get ))

//    println( sat.isVarsSat( 440 ) )
//
////    WCHAR 439  UCLIBC_HAS_LOCALE 207
//    println( sat.implication( 439, 207 ) )
//    println( sat.implication( 207, 439 ) )
////    c 515 UCLIBC_HAS_XLOCALE
//
//    println( sat.implication( 515, 207 ))
//    println( sat.implication( 515, 439 ))

  }

}

class ModelQuery( val dimacsFile: String, val extractFile: String ){

  val dh = DimacsReader.readHeaderFile( dimacsFile )
  val dp = DimacsReader.readFile( dimacsFile )
  val concreteKconfig = KConfigParser.parseKConfigFile( extractFile )

   val sat = new SATBuilder(dp.cnf, dp.numVars, dh.generated, dh.firstGen)
        with ImplBuilder with ConsoleLogger

  def implication( a: String, b: String ) =
    sat.implication( dh.idMap.get( a ).get, dh.idMap.get( b ).get )

}
