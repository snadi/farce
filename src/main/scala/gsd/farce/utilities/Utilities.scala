package gsd.farce.utilities

import java.io.FileInputStream
import java.util.Properties
import de.fosd.typechef.featureexpr.FeatureExpr
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 08/04/13
 * Time: 1:26 PM
 * To change this template use File | Settings | File Templates.
 */
object Utilities {

  private case class S( name: String, propertiesFile: String, sysConfig: Config )

  private val defaultSystem = S( "default", "busybox.properties", BB_GitServer )
  private val systems = S( "busybox", "busybox.properties", BB_GitServer ) ::
                        S( "linux", "linux.properties", Linux_2_6_33_3 ) ::
                      //  S( "axtls", "axtls.properties", axTLS_1_4_8 ) ::
                        S( "uclibc", "uclibc.properties", uclibc ) ::
                        S( "ecos", "ecos.properties", ecos ) ::
                        defaultSystem :: Nil

  def getSupportedSystems = systems.map( _.name )
  def getSupportedSystemsAsString = getSupportedSystems.reduceLeft[String]( _ + ", " + _ )

  // FIXME: should return new Properties instance to avoid state
  def loadPropertiesFile(system: String, properties: Properties) =
    properties load new FileInputStream(
      systems.find( _.name == system ) match{
        case Some( S(_, p, _ ) ) => p
        case _ => defaultSystem propertiesFile
      })

  def getSystemConfig( system: String ): Config =
     systems.find( _.name == system ) match{
       case Some( S( _, _, s ) ) => s
       case _ => defaultSystem sysConfig
     }

  def parseConstraint(constraintString: String) : FeatureExpr = {
    val input = new ANTLRInputStream( constraintString )
    val lexer = new FExprLexer( input )
    val tokens = new CommonTokenStream( lexer )
    val parser = new FExprParser( tokens )

    parser.fexpr.value
  }


  def percentage(count: Int, total:Int) : Double = math rint ((count.asInstanceOf[Double] / total.asInstanceOf[Double]) * 100)

}
