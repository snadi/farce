package gsd.farce.features.model.translation

import org.clapper.argot.{ArgotUsageException, ArgotConverters}
import gsd.linux._
import java.io.PrintStream
import gsd.linux.Id
import gsd.linux.Literal
import scala.Some
import gsd.linux.ConcreteKConfig
import gsd.linux.KInt
import gsd.linux.tools.ArgotUtil
import scala.util.logging.ConsoleLogger

/**
 * TODO: credits (copied from Steven's LVAT)
 */
object Kconfig2FormulaMain  extends ArgotUtil with ConsoleLogger {

  val name = "Kconfig2FormulaMain"

  import ArgotConverters._

  val inParam = parser.parameter[String](
    "in-file", "input Kconfig extract (.exconfig) file, stdin if not specified.", true)

  val outParam = parser.parameter[String](
    "out-file", "output file to write boolean expressions, stdout if not specified.", true)

  val noUndefinedFlag = parser.flag[Boolean](List("no-undefined"),
    "do NOT add constraints for undefined configs")

  val envParams = parser.multiOption[String](List("env"), "configname",
    "configs that have their values based on environment variables")

  def main(args: Array[String]) {

    try {
      parser.parse(args)

      val k =
      (pOpt.value, inParam.value) match {
        case (Some(_), Some(_)) =>
          parser.usage("Either a project (-p) is specified or input & output parameters are used.")

        case (Some(p), None) => p.exconfig

        case (None, Some(f)) =>
          log("Reading Kconfig extract from file...")
          KConfigParser.parseKConfigFile(f)

        case (None, None) =>
          log("Using stdin as input...")
          KConfigParser.parseKConfigStream(System.in)
      }

      val output =
      (pOpt.value, outParam.value) match {
        case (Some(p), None) => new PrintStream(p.boolFile.get)
        case (None, Some(f)) => new PrintStream(f)
        case _ => System.out
      }

      execute(k, output)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(k: ConcreteKConfig, out: PrintStream) {

    val ids = (k.identifiers map BExprUtil.sanitizeString).toSet

    assert(ids.size == k.identifiers.size)

    // TODO refactor this to a propositional formula class
    // First output identifiers
    for (id <- ids) {
      out.println("@ " + id)
      out.println("@ " + id + "_m")
    }

    var ak = k.toAbstractKConfig

    val additionalEnvs = envParams.value
    if (!additionalEnvs.isEmpty)
      ak = ak.copy(env = ak.env ::: additionalEnvs.toList)

    val addUndefined = !noUndefinedFlag.value.getOrElse(false)
    if (!addUndefined)
      log("Not adding constraints for undefined configs")

    val trans = new FarceTristateTranslation(ak, addUndefined)
    val exprs = trans.translate map (BExprUtil.sanitizeExpr)

    for (id <- trans.generated) out.println("$ " + id)

    // TODO move this to a BExprResult
    // variable configname value
    for ( ((name, value), id) <- trans.literalMap.toList sortBy { case ((x,_),_) => x })
      value match {
        case Literal(l) =>
          out.println("""$s %s %s %s""".format(id, name, l))
        case Id(x) =>
          out.println("""$v %s %s %s""".format(id, name, x))
        case KInt(i) =>
          out.println("""$i %s %s %s""".format(id, name, i))
        case KHex(h) =>
          out.println("""$h %s %s %s""".format(id, name, h))
        case e =>
          sys.error("Unsupported generated equality: " + e)
      }

    for (e  <- exprs) out.println(e)
  }
}
