package gsd.farce.features.model

import gsd.linux._
import gsd.linux.ConcreteKConfig
import gsd.linux.cnf._

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 30.04.13
 * Time: 17:49
 * To change this template use File | Settings | File Templates.
 */
object ModelQueryMain {

  var extract: ConcreteKConfig = null
  var satBuilder: Option[SATBuilder] = None

  def init( file: String ){
    if( file endsWith ".exconfig" )
      extract = KConfigParser.parseKConfigFile( file )
    else if( file endsWith ".dimacs" )
      satBuilder = Some( SATBuilder( file ) )

  }

  def main( args: Array[String] ){

    args match {
      case Array(i) => init( i )
      case _ => sys.error( "Usage: ModelQueryMain <extract file> [output file]")
    }

  }

  def toExpr{

    val trans = new TristateTranslation( extract.toAbstractKConfig )
    val exprs = trans.translate map (BExprUtil.sanitizeExpr)
    val features = extract.allConfigs.map( _.name ).toSet

    lazy val id2int = ( ( features ++ trans.generated.toSet ).
                          zipWithIndex map { case ( a, b ) => (a, b + 1 ) } ).toMap
    lazy val int2id = ( id2int map { case ( a, b ) => ( b, a ) } ).toMap

    val cnf = ( exprs ).flatMap( _.toCNF( id2int ) )

    println( cnf.toDimacs( int2id, trans.generated.toSet.map( id2int ) ) )

    exprs foreach println

  }

}
