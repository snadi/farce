package gsd.commons

import org.clapper.argot.{ArgotConverters, ArgotParser}

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 16.11.13
 * Time: 01:47
 * To change this template use File | Settings | File Templates.
 */
trait MyArgot {

  import ArgotConverters._

  val name: String
  val toolVersion: String

  lazy val parser = new ArgotParser(name, preUsage = Some( toolVersion ) )

}
