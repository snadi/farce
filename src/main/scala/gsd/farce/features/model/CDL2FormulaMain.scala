//package gsd.farce.features.model
//
//import gsd.cdl.model._
//import org.kiama.rewriting.Rewriter._
//import gsd.cdl.model.Node
//
///**
// * Created with IntelliJ IDEA.
// * User: berger
// * Date: 06.05.13
// * Time: 14:50
// * To change this template use File | Settings | File Templates.
// */
//case class FormulaElement( expr: CDLExpression, source: FormulaSource )
//
//class FormulaSource
//object Hierarchy extends FormulaSource
//object CrossTree extends FormulaSource
//object ORGroup extends FormulaSource
//object XORGroup extends FormulaSource
//object MUTEXGroup extends FormulaSource
//
//class CDL2FormulaTranslation( val model: IML ){
//
//  private implicit def string2Expression( s : String ):CDLExpression = Identifier( s )
//  private implicit def node2Expression( n : Node ):CDLExpression = Identifier( n.id )
//  private val TRUE: CDLExpression = True()
//  private val FALSE: CDLExpression = False()
//
//  def getFormula( topLevelNodes:List[Node] ): List[FormulaElement] = {
//
//    var constraintsPerNode = Map[String, List[CDLExpression]]()
//    val constraintConjuncts = collectl{
//      case n:Node => {
//        val constraints = rewriteConstraints( n )
//        constraintsPerNode += ( n.id -> constraints )
//        constraints.map( n.id implies _ )
//      }
//    }(topLevelNodes)
//
//    val hierarchyConjuncts = collectl{
//      case Node(n,_,_,_,_,_,_,_,_,_,_,children) =>  {
//        val optional = ( True /: children )( (a,b) => a & (b implies n) )
//        val mandat = children.filter( _.cdlType != InterfaceType ).filter( _.cdlType != PackageType).
//                      filter( x => x.flavor == NoneFlavor || x.flavor == DataFlavor ).
//                                        foldLeft( True )( (a,b) => a & ( (n & constraintsPerNode( b.id ) ) implies b ) )
//            optional & mandat
//          }
//      }(topLevelNodes)
//
//
//      // all top-level features are mandatory
//      // FIXME: not really, still depends on their cross-tree constraints
//      // so, we should add them, but currently, it's still sufficient....
//      val topLevelMandatoryConjuncts = topLevelNodes.map( _.id )
//
//      // process interfaces
//      val interfaceConjuncts = collectl{
//          case Node(n,InterfaceType,_,_,fl,_,_,_,_,_,_,_) if( fl == BoolFlavor || fl == BoolDataFlavor ) => {
//            // you can only understand the following if you're stoned (or drunk, depending on your preference)
//            ( n implies makeDisjunct( impls( n ) ) ) &
//            ( childParentMap( n ) & constraintsPerNode( n ) & makeDisjunct( impls( n ) ) ) implies n
//
//            // FIXME: childParentMap could return none, but haven't seen any interface as top-level node so far
//          }
//          case Node(n,InterfaceType,_,_,DataFlavor,_,_,_,_,_,_,_) => {
//            // any constraint on a data interface just affects the data value and is used for grouping
//            // thus, independent of its constraints and the state of the implementors, it is always
//            // active and enabled when the parent is (mandatory) -> thus, imposing its group constraint to the model
//            ( childParentMap( n ) ) implies n
//
//            // FIXME: childParentMap could return none, but haven't seen any interface as top-level node so far
//          }
//      }(topLevelNodes)
//
//      val conjuncts = hierarchyConjuncts ::: constraintConjuncts ::: interfaceConjuncts
//  //    val conjuncts = hierarchyConjuncts ::: constraintConjuncts ::: topLevelMandatoryConjuncts ::: interfaceConjuncts
//      val simplifiedConjuncts = conjuncts.map( rewrite( cleanupRule <* simplifyRule )( _ )  )  // try to further simplify; also
//                                                                                               // cleanup again (have some LoadedIdentifiers
//                                                                                               // still in there due to interface constraints
//      simplifiedConjuncts.filter( _ != True() )
//  }
//
//
//  def rewriteConstraints( n:Node ) =
//    ( n.activeIfs ::: n.reqs ) map { e =>
//      rewrite( resolveIdentifierRule <*
//               removeUnloadedIdentifierRule <*
//               simplifyRule <*
//               cleanupRule )(e)
//    }
//
//  val resolveIdentifierRule = everywheretd{
//    rule{
//      case Identifier( id ) => model.nodesById.get( id ) match {
//        case Some( n ) => LoadedIdentifier( id, n.cdlType, n.flavor )
//        case None => UnloadedIdentifier( id )
//      }
//    }
//  }
//
//  val removeUnloadedIdentifierRule = everywheretd {
//    rule{
//      case UnloadedIdentifier( id ) => False
//    }
//  }
//
//  val simplifyRule = everywherebu {
//    rule{
//      case Or( False(), e ) => e
//      case Or( e, False() ) => e
//      case Not( False() ) => True()
//      case Implies( e, True() ) => True()
//      case And( False(), _ ) => False()
//      case And( _, False() ) => False()
//      case And( e, True() ) => e
//      case Implies( False(), _ ) => True()
//      case Implies( e, False() ) => !e
//    }
//  }
//
//  val cleanupRule = everywheretd {
//    rule{
//      case LoadedIdentifier( id, _, _ ) => Identifier( id )
//    }
//  }
//
//
//
//  val interfaceReferenceFullRule = everywheretd {
//      rule{
//
//        case Eq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => !interf
//        //                    impls( id ).foldLeft( True():CDLExpression )( (a,b) => a & !b )
//
//        //      case GreaterThan( LoadedIdentifier( id, InterfaceType, _ ), IntLiteral( 0 ) ) =>
//        //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//        //      case NEq( LoadedIdentifier( id, InterfaceType, _ ), IntLiteral( 0 ) ) =>
//        //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//        //      case NEq( IntLiteral( 0 ), LoadedIdentifier( id, InterfaceType, _ ) ) =>
//        //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//
//
//        // or groups
//        // here, it's sufficient to reference the interface node, since the or constraint is already
//        // incorporated in the interface constraint, introduced in the getFormula() method
//        case GreaterThan( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => interf
//        case GreaterThanOrEq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 1 ) ) => interf
//        case NEq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => interf
//        case NEq( LongIntLiteral( 0 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) => interf
//
//        // mutex group: interface <= 1
//        // don't reference the interface symbol here!
//        case GreaterThanOrEq( LongIntLiteral( 1 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) =>
//                      mutex( impls( id ) )
//
//        case Eq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 1 ) ) =>
//          interf & xor( impls( id ) )
//        case Eq( LongIntLiteral( 1 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) =>
//          interf & xor( impls( id ) )
//
//      }
//    }
//
//  val interfaceReferenceBoolRule = everywheretd {
//    rule{
//
//      // For these interfaces, it's important to always reference the interface symbol in the conjunction
//      // as well, since the interface can have additional constraints (at least the parent constraint)
//      // that could disable it, In this case, the states of all the nodes implementing the interface don't
//      // play a role at all.
//
//      case Eq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => !interf
//      //                    impls( id ).foldLeft( True():CDLExpression )( (a,b) => a & !b )
//
//      //      case GreaterThan( LoadedIdentifier( id, InterfaceType, _ ), IntLiteral( 0 ) ) =>
//      //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//      //      case NEq( LoadedIdentifier( id, InterfaceType, _ ), IntLiteral( 0 ) ) =>
//      //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//      //      case NEq( IntLiteral( 0 ), LoadedIdentifier( id, InterfaceType, _ ) ) =>
//      //                    impls( id ).foldLeft( False():CDLExpression )( (a,b) => a | b )
//
//
//      // or groups
//      // here, it's sufficient to reference the interface node, since the or constraint is already
//      // incorporated in the interface constraint, introduced in the getFormula() method
//      case GreaterThan( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => interf
//      case GreaterThanOrEq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 1 ) ) => interf
//      case NEq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 0 ) ) => interf
//      case NEq( LongIntLiteral( 0 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) => interf
//
//      // mutex group: interface <= 1
//      // don't reference the interface symbol here!
//      case GreaterThanOrEq( LongIntLiteral( 1 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) =>
//                    mutex( impls( id ) )
//
//      case Eq( interf@LoadedIdentifier( id, InterfaceType, _ ), LongIntLiteral( 1 ) ) =>
//        interf & xor( impls( id ) )
//      case Eq( LongIntLiteral( 1 ), interf@LoadedIdentifier( id, InterfaceType, _ ) ) =>
//        interf & xor( impls( id ) )
//
//    }
//  }
//
//  def makeConjunct( l : List[CDLExpression] ) =
//    ( True /: l )( _ & _ )
//
//  def makeDisjunct( l : List[CDLExpression] ) =
//    ( False /: l )( _ | _ )
//
//  def xor( ids : List[LoadedIdentifier] ) = {
//    val disjuncts = for( i <- ids ) yield ( i /: ids.filter( _ != i ))( _ & !_ )
//    makeDisjunct( disjuncts )
//  }
//
//  def mutex( ids : List[LoadedIdentifier] ) = {
//    val disjuncts = for( i <- ids ) yield i implies ids.filter( _ != i ).foldLeft( True )( _ & !_ )
//    makeConjunct( disjuncts )
//  }
//
//
//}
//
//
//object CDL2FormulaMain {
//
//
//
//}