package gsd.farce.features.model

import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader
import org.sat4j.tools.xplain.Xplain
import org.sat4j.core.VecInt
import scala.collection.JavaConversions._
import scala.math._
import org.sat4j.minisat.constraints.cnf.{Lits, BinaryClause, WLClause}
import org.sat4j.tools.DimacsOutputSolver
import java.io.{PrintWriter, FileWriter, FileOutputStream}
import org.sat4j.specs.{IConstr, IVecInt}
import org.sat4j.minisat.core.{DataStructureFactory, Solver}
import scala.collection.immutable.HashSet

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 05.09.13
 * Time: 15:26
 * To change this template use File | Settings | File Templates.
 */
object UnsatCoreMainAlternative {

  implicit def iConstr2MyConstr( c: IConstr ) = new MyConstr( c )

  def main(args:Array[String]){

    val outputViaDimacsSolver = args match {
      case Array("-useDimacsSolver") => true
      case _ => false
    }

    println("Starting")
    val typeModel = SATFeatureModel.createFromDimacsFile(args(0), "", "")
    println("INFO: created sat feature model from dimacs")
    val solver = SolverFactory.newDefault()

    val int2StringMap = typeModel.variables.map{ case (x,y) => (y,x) }.toMap
    int2StringMap.keys foreach{ k =>
      solver.registerLiteral( k )
    }

    val x = new Xplain( solver, false )

    for( i <- 0 to typeModel.clauses.size-1 ){
      val c = typeModel.clauses.get( i )
      x.addClause( c )
    }

    println("added clauses and going to test sat")

//    println( x.addClause( new VecInt( Array(105) ) ) )
//    println( x.addClause( new VecInt( Array(-105) ) ) )
//
//    println( x.addClause( new VecInt( Array(49) ) ) )
//    println( x.addClause( new VecInt( Array(-49) ) ) )

    println( x.isSatisfiable )

    println("passed the first satisfiable")

    var toRemoveClauseIndex = Set[Int]()

    var count = 1

    while( !x.isSatisfiable ){

      println("looping " + count)
      count += 1

      val explain = x.explain()
      println( explain )


//      val mE = x.minimalExplanation()
//      for( i <- mE )
//        println( i )

      for( ex <- explain ){

        for( i <- 0 to ex.size-1 ){ // omit the selector variable!
        val v = Lits.toString( ex.get(i) ).toInt
          if( int2StringMap.containsKey( abs(v) ) ){
            if( v < 0 )
              print("-")
            print( int2StringMap.get( abs(v) ) match{
              case Some(n) => n
              case None => abs(v)
            } )
            print( " " )
          }
        }

//        toRemoveClauseIndex += ex
        x.removeConstr( ex )
        println
      }

    }

    println( x.isSatisfiable )

    val logOut = new PrintWriter(new FileWriter("output/vars_used.log"))

    println("going to write to output file: " + args(1))
    val pw = new PrintWriter (new FileWriter(args(1)))


    val castSolver = solver.asInstanceOf[Solver[DataStructureFactory]]
    println( "number of vars: " + castSolver.nVars )

//    val originalLits = new HashSet[Int]( int2StringMap )
    val maxOriginalLit = (0 /: int2StringMap.keySet)( max )
    println( "max original literal: " + maxOriginalLit )

    var litsUsed = Set[Int]()
    for( i <- 0 to solver.nConstraints() -1 ){
      val c = castSolver.getIthConstr( i )
      var selectorFound = false
      for( i <- 0 to c.size-1 ){
        val v = (c.get(i) >> 1)
        if( v <= maxOriginalLit ){
          litsUsed += v
          logOut.println( v )
        }else
          selectorFound=true
      }
      if(!selectorFound)
        sys.error("no selector var found in " + c )
    }

    int2StringMap foreach { case (i,s) =>
      if ( litsUsed.contains(i) ){
        if( s.startsWith("__") )
          pw.println("c " + i + "$ " + s )
        else
          pw.println("c " + i + " " + s )
      }
    }

    pw.println( "p cnf " + litsUsed.size + " " + solver.nConstraints() )

    for( i <- 0 to solver.nConstraints() -1 ){
      val c = castSolver.getIthConstr( i )
      for( i <- 0 to c.size-1 ){
        val v = (c.get(i) >> 1)
        if( v <= maxOriginalLit ){
          pw.print( Lits.toString( c.get(i) ) )
          pw.print(" ")
        }
      }
      pw.println("0")
    }


    logOut.close

    pw.close


  }

  def getSelectorVar( c: IConstr, i2s: Map[Int,String] ): Int = {
    for( i <- 0 to c.size-1 ){
      val v = (c.get( i ) >> 1)
      if( !i2s.containsKey( v ) )
        return v
    }
    sys.error("Could not find selector var in constraint " + c )
  }

  def constraintToVecInt( c: IConstr, int2StringMap: Map[Int, String] ) = {
    val ret = new VecInt(c.size-1)
    for( i <- 0 to c.size-1 ){
//      val v = Lits.toString( c.get(i) ).toInt
      val lit = c.get(i)
      val absV = lit >> 1
      val v = (if ((lit & 1) == 0) 1 else -1) * (lit >> 1)
      if( int2StringMap containsKey absV ){
        ret.push( v )
      }
    }
    ret
  }

  class MyConstr(val c: IConstr ){

    override def equals( other: Any ): Boolean = {
      if( other.isInstanceOf[MyConstr] ){
        val o = other.asInstanceOf[MyConstr]
        if( o.c.size != c.size )
          return false
        for( i <- 0 to c.size() )
          if( o.c.get(i) != c.get(i) )
            return false
        return true
      }else
        return false
    }

  }

}
