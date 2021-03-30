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

/**
 * Created with IntelliJ IDEA.
 * User: berger
 * Date: 05.09.13
 * Time: 15:26
 * @deprecated use UnsatCoreMainAlternative
 */
object UnsatCoreMain {

  implicit def iConstr2MyConstr( c: IConstr ) = new MyConstr( c )

  def main(args:Array[String]){

    val outputViaDimacsSolver = args match {
      case Array("-useDimacsSolver") => true
      case _ => false
    }

    val typeModel = SATFeatureModel.createFromDimacsFile("input/typeErrorFeatureExpr_small.dimacs", "", "")
    val solver = SolverFactory.newDefault()

    val int2StringMap = typeModel.variables.map{ case (x,y) => (y,x) }.toMap
    int2StringMap.keys foreach{ k =>
      solver.registerLiteral( k )
    }

//    val reader = new DimacsReader( solver )

    val x = new Xplain( solver, false )

    for( i <- 0 to typeModel.clauses.size-1 ){
      val c = typeModel.clauses.get( i )
      x.addClause( c )
    }

    println( x.addClause( new VecInt( Array(105) ) ) )
    println( x.addClause( new VecInt( Array(-105) ) ) )

    println( x.addClause( new VecInt( Array(49) ) ) )
    println( x.addClause( new VecInt( Array(-49) ) ) )

    println( x.isSatisfiable )

    var toRemove = Set[MyConstr]()
    var toRemoveSelVar = Set[Int]()

    while( !x.isSatisfiable ){

      val explain = x.explain
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
        x.removeConstr( ex )
//        toRemove += ex  // awkward, but x.getConstraints returns all clauses, regardless of selector variable
//        if( toRemoveSelVar contains getSelectorVar( ex ) )
//          sys.error("duplicate selector var")
        toRemoveSelVar += getSelectorVar( ex )
        println
      }

    }

    println( x.isSatisfiable )

    val logOut = new PrintWriter(new FileWriter("output/xplain_faster.log"))

    val pw = new PrintWriter (new FileWriter("output/satisfiable_linux_type_formula.dimacs"))

    int2StringMap foreach { case (i,s) =>
      if( s.startsWith("__") )
        pw.println("c " + i + "$ " + s )
      else
        pw.println("c " + i + " " + s )
    }

    val allC =  x.getConstraints

    if( outputViaDimacsSolver ){

      val outputSolver = new DimacsOutputSolver(pw) //SolverFactory.newDimacsOutput()

      outputSolver.newVar( int2StringMap.size )  // solver.nVars returns also all selector variables
      outputSolver.setExpectedNumberOfClauses( allC.size() - toRemoveSelVar.size )

      println( "allC size: " + allC.size )

      allC foreach { c =>
        if( !(toRemoveSelVar contains getSelectorVar( c )) ){
          outputSolver.addClause( constraintToVecInt( c, int2StringMap ) )
          //logOut.println ("constraint: " + c)
        }
      }

    }else{
      pw.println( "p cnf " + int2StringMap.size + " " + (allC.size() - toRemoveSelVar.size) )
      allC foreach { c =>
        if( !(toRemoveSelVar contains getSelectorVar( c )) ){
          for( i <- 0 to c.size-2 ){
            pw.print( Lits.toString( c.get(i) ) )
            pw.print(" ")
          }
          pw.println("0")
        }
      }
    }

    logOut.close

    pw.close


  }

  def getSelectorVar( c: IConstr ): Int =
    c.get( c.size -1 )

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
