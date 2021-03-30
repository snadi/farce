package gsd.farce.features.model.patterns

import java.io.PrintStream

import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import akka.util.Duration._
import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import gsd.farce.features.model.FeatureRelationshipsMain._
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ISolver
import org.sat4j.core.VecInt
import scala.io.Source
import akka.util.{Timeout, Duration}
import java.util.concurrent.{Executors, TimeUnit}
import akka.pattern.{ ask, pipe }
import akka.dispatch.{ExecutionContext, Promise, Future, Await}
import gsd.commons.MyArgot
import gsd.farce.FarceMain
import org.clapper.argot.{ArgotUsageException, ArgotConverters}

/**
 * Created by berger on 23/06/2014.
 */


case class ImplicationsToCheck( toCheck: Seq[Tuple2[Int,Int]], model: String )
case class ImplicationsThatHold( impls: Seq[Tuple2[Int,Int]] )
case class BuildImplicationGraph( model: String, processors: Int )
case class Progress( done: Int, total: Int )
case class PrintProgress()
case class Total( t: Int )

class ImplicationCheckWorker extends Actor{


  var solver:Option[ISolver] = None

  def implication(v1: Int, v2: Int): Boolean = {
    !isSatisfiable(List(v1, -v2))
  }

  def toVecInt(lits: Iterable[Int]): VecInt = new VecInt(lits.toArray)

  def isSatisfiable(assump: Iterable[Int]) =
      solver.get.isSatisfiable(toVecInt(assump))

  def receive = {
    case ImplicationsToCheck( toCheck, model ) => {
      val fm = SATFeatureModel.createFromDimacsFile( model, "", "")
      val int2StringMap = fm.variables.map{ case (x,y) => (y,x) }.toMap
      solver = Some( SolverFactory.newDefault() )
      println( "new solver instance: " + System.identityHashCode( solver.get ) )
      int2StringMap.keys foreach{ k =>
        solver.get.registerLiteral( k )
      }
      solver.get.addAllClauses( fm.clauses )

      var found:Set[Tuple2[Int,Int]] = Set.empty

      var c = 0
      var total = toCheck.size
      toCheck foreach { case(i,j) => {
        c += 1
        if( implication(i,j) )
          found += ( (i,j) )
        val updateActor = context.system.actorFor( "akka://implication-graph-system/user/printProgress" )

        if( c % 100 == 0 ) // don't send too many messages
          updateActor ! Progress( c, total )
      }}

      sender ! ImplicationsThatHold( found.toSeq )

    }
  }

}

class ImplicationCheckMaster extends Actor{

  implicit val timeout = Timeout( Duration( 1, "days" ) )
  implicit val ec = context.system.dispatcher

  val vardef  = """(?m)^c (\d+)(\$?) (\w+)(?: (.) (\d+) (.+))?$""".r
  var total = -1
  var workers = Seq[ActorRef]()

  def receive = {

    case BuildImplicationGraph( m, processors ) => {
      val model = SATFeatureModel.createFromDimacsFile( m, "", "" )

      val generated = Source.fromFile(m).getLines.flatMap{ _ match{
        case vardef(v, isGenSym, id, eqType, eqVar, eqValue) if isGenSym != "" => v.toInt :: Nil
        case _ => Nil
      }} toSet
      val int2StringMap = model.variables.map{ case (x,y) => (y,x) }.toMap

      val featureMap = model.variables.filterNot( generated contains _._2 ).filterNot( _._1 endsWith "_m" )
      val reverseFeatureMap = featureMap.map{ case (x,y) => (y,x) }

      println( "# of features: " + featureMap.size )

      var combinationList:List[Tuple2[Tuple2[String,Int],Tuple2[String,Int]]] = Nil
      for( i <- featureMap; j <- featureMap )
        if( i._2 != j._2 )
          combinationList = (i,j) :: combinationList

      total = combinationList.size
      workers = createWorkers( processors )

      val combinationListWithIndex = combinationList.map{ case (a,b) => (a._2,b._2) }.zipWithIndex
      val clusters = for( i <- 0 to processors-1 )
        yield combinationListWithIndex.filter( c => ( ( c._2 % processors ) == i ) )


      val futures =
        ( for( i <- 0 to workers.size-1 ) yield {
          val cluster = clusters(i).map( _._1 )
          ask( workers(i), ImplicationsToCheck( cluster, m ) ).mapTo[ImplicationsThatHold]
        } )

      val updateActor = context.system.actorFor( "akka://implication-graph-system/user/printProgress" )
      updateActor ! Total( total )

      // back to feature name
      val toFeatureName: Tuple2[Int,Int] => Tuple2[String,String] = { case(a,b) =>
        ( reverseFeatureMap.get(a).get, reverseFeatureMap.get(b).get )
      }

      val singleFuture = Future.sequence( futures )
      singleFuture.map( r => r.map( _.impls.map( toFeatureName ) ).flatten ) pipeTo sender

    }

    case s:String => println( s )

  }

  private def createWorkers(numActors: Int) = {
    for (i <- 0 until numActors) yield context.actorOf(Props[ImplicationCheckWorker], name = "worker-" + i )
  }

}

class PrintProgressActor extends Actor{

  var actorProgress = Map[ActorRef,Int]()
  var total = -1

  def receive = {

    case Total( t ) =>
      total = t

    case Progress( c, _ ) => {
          actorProgress += ( sender -> c )
        }

    case PrintProgress => {
      val done = ( 0 /: actorProgress.values)( _ + _ )
      val percent = ( done.toFloat / total.toFloat ) * 100
      Console.print( done + " / " + total + " (" + percent.toInt + "%)\r" )
    }
  }
}


object ParallelImplicationGraphMain extends MyArgot{

  import ArgotConverters._

  val name = "ParallelImplicationGraphMain"
  val toolVersion = "ParallelImplicationGraphMain " + FarceMain.globalVersionString
  implicit val timeout = Timeout( Duration(1, "days") )

  val processors = parser.option[Int]("p","n","number of parallel threads")
  val configprefix = parser.flag[Boolean]( "c", false, "add CONFIG_ prefix to feature names")
//  {
//    (onOff, opt) => if (onOff) "CONFIG_" + 1 else ""
//  }
  val input = parser.parameter[String]("dimacs", "constraints as a Dimacs file", false)
  val output = parser.parameter[String]("implGraph", "implication graph file", true)

  def main(args: Array[String]) {


    try {
      parser.parse(args)

      val system = ActorSystem("implication-graph-system")
      val m = system.actorOf(Props[ImplicationCheckMaster], name="master")

      val p = processors.value.getOrElse( Runtime.getRuntime.availableProcessors() )
      println( "using " + p + " processors" )

      val start = System currentTimeMillis

      val future = ( m ? BuildImplicationGraph( input.value.get, p) )

      val updateActor = system.actorOf( Props[PrintProgressActor], "printProgress" )
      val d = Duration.create(1000, TimeUnit.MILLISECONDS)
      val cancellable = system.scheduler.schedule( d, d, updateActor, PrintProgress )

      val result = Await.result( future, Duration.Inf ).asInstanceOf[Seq[Tuple2[String,String]]]

      println( "number of implications: " + result.size )

      cancellable.cancel
      val end = System.currentTimeMillis
      system.shutdown
      println( "took " + time( end - start ) )

      val out = output.value match {
        case Some( f ) => new PrintStream(f)
        case None => System.out
      }
      val pre = if( configprefix.value != None ) "CONFIG_" else ""
      result.foreach { case (a,b) =>
        out.println( pre + a + " => " + pre + b )
      }

      out close

    }catch{
      case e:ArgotUsageException => println( e.getMessage )
    }



  }

  def time( t: Long ) =
    TimeUnit.MILLISECONDS.toMinutes( t ) +
      " min, " +
      ( TimeUnit.MILLISECONDS.toSeconds( t ) -
        TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes( t ) ) ) +
      " sec"

}