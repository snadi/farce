package gsd.farce.features

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureExprParser}
import de.fosd.typechef.featureexpr.sat.{SATFeatureModel, SATFeatureExpr}
import java.io.{File, FileWriter}
import FeatureExprFactory.sat._
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import io.Source
import gsd.farce.features.FeatureUtils._
import de.fosd.typechef.featureexpr.antlr.{FExprParser, FExprLexer}

/**
 * reads a feature expression from a file (parameter 1) and creates a dimacs file (parameter 2)
 * Copied from Kaestner's TypeChef-BusyBoxAnalysis
 */


object CreateDimacs {

  def main(args: Array[String]) {

    if (args.length < 2) {
      println(
        """
          |Invalid parameters:
          | --cnf or --equicnf (mandatory) selecting between CNF and Equisatisfiable CNF transformations
          | input file (mandatory)
          | prefix to be check for (e.g., CONFIG_) etc.
          | output file (optional) default is fm.dimacs
        """.stripMargin)
    }
    else {
      val isCNF = args(0) != "--equicnf"

      val inputFilename = args(1)
      val prefix = if (args.length > 2) args(2) else ""
      val suffix = if (args.length > 3) args(3) else ""
      assert(new File(inputFilename).exists(), "File " + inputFilename + " does not exist")

      val lines = Source.fromFile(inputFilename).getLines()
      var input = new ANTLRInputStream(lines.next().trim)
      System.err.println("READ EXPR")
      var lexer = new FExprLexer(input)
      var tokens = new CommonTokenStream(lexer)
      var parser = new FExprParser(tokens)
                                   System.err.println("PARSED EXPR")
      val fexpr = parser.fexpr().value.asInstanceOf[SATFeatureExpr]
      System.err.println("have fexpr")
      val outputFilename = if (args.length < 4) "fm.dimacs" else args(4)
      createDimacs(fexpr, outputFilename, isCNF, prefix, suffix)
    }
  }

  def createDimacs(fexpr: SATFeatureExpr, outputFileName: String, isCNF: Boolean, prefix: String, suffix: String){
    val fm = SATFeatureModel.create(if (isCNF) fexpr else fexpr.toCnfEquiSat()).asInstanceOf[SATFeatureModel]


    val out = //new OutputStreamWriter())
      new FileWriter(outputFileName)

    for ((v, i) <- fm.variables) {
      out.write("c " + i + (if (v.startsWith(prefix) && v.endsWith(suffix)) " " else "$ ") + v + "\n")
    }

    out.write("p cnf " + fm.variables.size + " " + fm.clauses.size() + "\n")

    var i = 0
    while (i < fm.clauses.size) {
      val c = fm.clauses.get(i)
      val vi = c.iterator()
      while (vi.hasNext)
        out.write(vi.next + " ")
      out.write("1\n")
      i = i + 1
    }

    out.close()
  }
}