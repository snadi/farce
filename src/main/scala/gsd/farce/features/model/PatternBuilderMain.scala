package gsd.farce.features.model

import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import org.sat4j.minisat.SolverFactory
import gsd.linux.cnf.{DoneArray, DimacsReader, ImplBuilder, SATBuilder}
import scala.util.logging.ConsoleLogger
import org.sat4j.reader.DimacsReader
import scala.io.Source
import org.sat4j.core.VecInt
import gsd.graph.{Edge, DirectedGraph}
import scala.actors.Actor._

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 28.11.13
 * Time: 21:17
 * To change this template use File | Settings | File Templates.
 */
object PatternBuilderMain{

  def main( args: Array[String] ){

    val model = SATFeatureModel.createFromDimacsFile(args(0), "", "")
    val f = args(0)


    val vardef  = """(?m)^c (\d+)(\$?) (\w+)(?: (.) (\d+) (.+))?$""".r
    val generated = Source.fromFile(f).getLines.flatMap{ _ match{
      case vardef(v, isGenSym, id, eqType, eqVar, eqValue) if isGenSym != "" => v.toInt :: Nil
      case _ => Nil
    }} toSet
    val int2StringMap = model.variables.map{ case (x,y) => (y,x) }.toMap



    val featureMap = model.variables.filterNot( generated contains _._2 ).filterNot( _._1 endsWith "_m" )

    println( "feature: " + featureMap.size )
    featureMap.foreach( println )

    val cnf = for( i <- 0 to model.clauses.size-1 ) yield {
      val c = model.clauses.get( i )
      val intArray = new Array[Int]( c.size )
      c.copyTo( intArray )
      intArray.toList

    }


    var combinationList:List[Tuple2[Tuple2[String,Int],Tuple2[String,Int]]] = Nil
    for( i <- featureMap; j <- featureMap )
      if( i._2 != j._2 )
        combinationList = (i,j) :: combinationList

    val processors = 3
    val combinationListWithIndex = combinationList.zipWithIndex
    val clusters = for( i <- 0 to processors-1 ) yield combinationListWithIndex.filter( c => ( ( c._2 % processors ) == i ) )



    def toVecInt(lits: Iterable[Int]): VecInt =
        new VecInt(lits.toArray)

    var actorCount = 1
    var done = 0
    val size = combinationList.size

    val actors = clusters.map{ c =>
      actor{

        val actorNumber = actorCount
        actorCount += 1

        val solver = SolverFactory.newDefault()

        int2StringMap.keys foreach{ k =>
          solver.registerLiteral( k )
        }
        solver.addAllClauses( model.clauses )


        def implication(v1: Int, v2: Int): Boolean = {
          println( "actor" + actorNumber +  " testing: " + v1 + ", " + v2 )
          println( "testing " + done + "/" + size )
          done += 1
    //      println( "time: " + ( System.currentTimeMillis() / 1000 ) )
              !isSatisfiable(List(v1, -v2))
            }

        def isSatisfiable(assump: Iterable[Int]) =
            solver.isSatisfiable(toVecInt(assump))


        c.map( _._1 ) foreach { case(i,j) =>
          implication(i._2,j._2)
        }
      }
    }



//    val startTime = System.currentTimeMillis()
//    var c = 0
//    val size = combinationList.size
//    val result = combinationList.par.filter{ case (i,j) => {
////      c+=1
////      if( c % 5000 == 0 )
////        println( "checking time for 5000 combinations: " + ( System.currentTimeMillis() - startTime ) )
////      println( "testing " + c + "/" + size )
////      println(c)
//      implication(i._2,j._2)
//    } }

//    println(result)




//    val sat = new SATBuilder( cnf, model.variables.size, generated ) with FarceImplBuilder with ConsoleLogger
//    val g = sat.mkImplicationGraph( featureMap )



//    val g = sat.mkImplicationGraph(  )
//    println( g.edges.size )
//    println( g.vertices.size )

//    println( g.toParseString )
//    val g = sat.mkImplicationGraph(header.varMap, Nil)



  }


}

trait FarceImplBuilder extends SATBuilder with DoneArray {

  /**
   * Same effect as isSatisfiable, but with a different name to avoid problems
   * with type erasure.
   */
  def isVarsSat(vs: Int*) = solver.isSatisfiable(new VecInt(vs.toArray))

  /**
   * Returns true iff v1 implies v2, false otherwise
   */
  def implication(v1: Int, v2: Int): Boolean = {
    println( "testing: " + v1 + ", " + v2 )
    !isSatisfiable(List(v1, -v2))
  }

  /**
   * Dead features should be removed prior to calling this otherwise these
   * dead features will have implications to all items features!
   *
   * Optimization taken from Nele's implementation: If the formula is
   * satisfiable after a check to implication, then we examine the resulting
   * model. In that model, if there exists i = TRUE, and j = FALSE, then we
   * know that i does NOT imply j. Look at the truth table for implication.
   *
   */
  def mkImplicationGraph[T]( featureMap: Map[String,Int] ): Set[Tuple2[String,String]] = {

    val startTime = System.currentTimeMillis()

//    val combinationList = for( i <- featureMap; j <- featureMap; if i._2 != j._2 ) yield (i,j)
    var combinationList:List[Tuple2[Tuple2[String,Int],Tuple2[String,Int]]] = Nil
    for( i <- featureMap; j <- featureMap )
      if( i._2 != j._2 )
        combinationList = (i,j) :: combinationList

    val parList = combinationList.toList.par
    println( "combination list size: " + combinationList.size )

    val result = parList.filter{ case (i,j) => implication(i._2,j._2) }


    Console.println("Done after %3d s!".format(
      (System.currentTimeMillis() - startTime) / 1000
    ))

//    val r = result.map( a => ( a._1._1, a._2._1 ) )
    var r: Set[Tuple2[String,String]] = Set()
    result.foreach{ case (i,j) => r += ((i._1, j._1)) }

    r
  }
}