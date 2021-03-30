package gsd.farce.filepcs

import gsd.cdl.parser.EcosIml
import gsd.cdl.model.PackageType

/**
 * Created by snadi on 30/06/14.
 */
object CDLFilePcExtractor {
   /*
   - extracts the compile constraint from CDL files to output the file pcs
   - uses information from ecos.db to get the directory mapping to packages (directory paths)
    */

  def main(args: Array[String]) {
    val model = EcosIml.CupParser.parseFile(args(0))
    val childParentMap = model.childParentMap
    var filePcMap = Map[String, String]()

    var definesMap = model.defines

    val dirMap = EcosDBExtractor.getMap("ecos/packages/ecos.db")

    model.allNodes.filter( x => x.compileConstraints != null && x.compileConstraints.size > 0).foreach(
      node => {
        var path = ""
        var parent = ""
        var parentConstraint= ""

        if(node.cdlType == PackageType){
          //if it is a package itself then it will have a directory in the map
          path = dirMap.getOrElse(node.id, "")

        }else{
          //else get the path from the parent (which is probably a package..
          parent = node.id
          do{
            parent = childParentMap.getOrElse(parent, "")
           // println("parent: " + parent)
            //println("type: " + model.nodesById.get(parent).get.cdlType)
          } while(model.nodesById.get(parent).get.cdlType != PackageType) //we must reach a package to get a path

          parentConstraint = definesMap.getOrElse(parent, parent)  //if there's a define for it, use the define
                                                                   //otherwise use id
          path = dirMap.getOrElse(parent, "")
        }

        node.compileConstraints.foreach(file => {
          if(file.endsWith(".c")){
            val fullFile = path + file
            val nodeName = definesMap.getOrElse(node.id, node.id)  //if there's a define for it, use the define
                                                                  //otherwise use id
            var constraint = ""
            if(parentConstraint.isEmpty)
              constraint = "(" + nodeName  + " == \"y\")"
            else
              constraint  = "((" +  nodeName + " == \"y\") && (" + parentConstraint + " == \"y\"))"

            constraint  = filePcMap.get(fullFile) match{
              case Some(n) => "(" + n + " || " + constraint + ")"
              case None => {//there is already an entry so create a disjunction
                constraint
              }
            }

            filePcMap +=  (fullFile -> constraint )
          }
        })
      }
    )

     filePcMap.foreach(entry => println(entry._1 + ": " + entry._2))
  }
}
