import gsd.farce.features.model.{FeatureRelationships, FeatureRelationshipsMain}
import org.scalatest.FunSuite

/**
 * Created by berger on 06.12.13.
 */
class FeatureRelationshipsTest extends FunSuite{

  test("There should be no hierarchy edges in the set of cross-tree edges."){
    FeatureRelationships.getFeatureRelationshipFinder( this.getClass.getResource( "uclibc/uClibc-0.9.33.2.exconfig" ).getPath )
    // TODO: testcase
  }

}
