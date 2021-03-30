package gsd.farce.features.model

import gsd.linux._
import gsd.cdl.tse11.TSE11Statistics
import gsd.cdl.model.IML
import gsd.cdl.model.IML

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 04.09.13
 * Time: 23:21
 * To change this template use File | Settings | File Templates.
 */
object FeatureTypesMain {

  def main(args:Array[String]){
    args match {
      case Array( f ) => {
        getDataFeatures( f ) foreach println
      }
      case _ => sys.error( "Usage: FeaturesTypesMain <model file>")
    }
  }

  def getDataFeatures( f: String ): Set[String] =
    if( f endsWith ".iml" )
      getCDLDataFeatures( f )
    else if ( f.endsWith(".exconfig") || f.endsWith(".pb") )
      getKconfigDataFeatures( f )
    else
      sys.error( "Unknown file type: " + f )

  def getKconfigDataFeatures( f: String ): Set[String] =
    getKconfigDataFeatures( KConfigParser parseKConfigFile f )

  def getKconfigDataFeatures( ck: ConcreteKConfig ) = {
    val datas = KStringType :: KIntType :: KHexType :: Nil
    ck.allConfigs.filter( datas contains _.ktype ).map( _.name ).toSet
  }

  def getCDLDataFeatures( model: IML ) =
    new TSE11Statistics( model ).dataFeatures.map( _.id ).toSet

  def getCDLDataFeatures( f: String ): Set[String] = {
    val es = TSE11Statistics( f )
    es.dataFeatures.map(_.id).toSet
  }


}
