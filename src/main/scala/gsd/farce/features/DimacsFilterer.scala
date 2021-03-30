package gsd.farce.features

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 11/09/13
 * Time: 2:49 PM
 * To change this template use File | Settings | File Templates.
 */
object DimacsFilterer {


  def main(args: Array[String]) {
    val inputLines = io.Source.fromFile(args(0)).getLines().toList
    val FILTER_SIZE = args(1).toInt



    for (file <- inputLines){
      var numOfFeatures = 0
      var numOfGenerated = 0
      var totalNumOfVars = 0
      val source = scala.io.Source.fromFile(file)
      for (line <- source.getLines) {
         if (line.startsWith("p cnf")){
           val parts = line.split(" ")
           //p cnf numOfVars
           totalNumOfVars = parts(2).toInt
         }else if (line.startsWith("p ")){
           val parts = line.split(" ")
           //p numOfVars
           totalNumOfVars = parts(1).toInt
         } else if (line.startsWith("c ")){
           val parts = line.split(" ")
           val id = parts(1)
           val feature = parts(2).trim()

           if (!id.endsWith("$") && !id.startsWith("$")) {
             numOfFeatures +=1
           }else{
             numOfGenerated +=1
           }
         }
      }

      if (numOfFeatures <= FILTER_SIZE){
        println(file)
      } else{
        System.err.println("Filter: " + file + " with " + totalNumOfVars + " variables, " + numOfFeatures + " features and " + numOfGenerated + " generated")
      }
    }
  }

}
