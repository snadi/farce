package testing

import de.fosd.typechef.featureexpr.sat.SATFeatureModel
import de.fosd.typechef.featureexpr.FeatureExprParser
import org.sat4j.specs.IProblem
import org.sat4j.reader.DimacsReader
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import gsd.farce.features.FeatureUtils


/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 04/09/13
 * Time: 7:48 AM
 * To change this template use File | Settings | File Templates.
 */
object ReadingDimacsTest extends App{

  val solver = SolverFactory.newDefault();
  solver.setTimeout(3600); // 1 hour timeout
  val reader = new DimacsReader(solver);
  // CNF filename is given on the command line

    val problem = reader.parseInstance(args(0))
  if (problem.isSatisfiable()) {
    System.out.println("Satisfiable !");
    System.out.println(reader.decode(problem.model()));
  } else {
    System.out.println("Unsatisfiable !");
  }

  val varMap = FeatureUtils.getVariableMap(args(0))

  val variable1 = varMap.get(args(1)).get
  val variable2 = varMap.get(args(2)).get


  val assumptions = new VecInt();

  assumptions.push(variable1)
  assumptions.push(variable2 * -1)

  if (!problem.isSatisfiable(assumptions)){
    System.out.println("A => B")
  }else{
    System.out.println("does not imply")
  }

 /* println(test1.toString() + " can be satisfied: " + test1.isSatisfiable(typeModel))

  println(test2.toString() + " can be satisfied: " + test2.isSatisfiable(typeModel))
  println(test3.toString() + " can be satisfied: " + test3.isSatisfiable(typeModel))*/

}
