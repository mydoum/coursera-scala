package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  test("Can we open station the station file") {
    val bufferedSource = Extraction.openStationFile("/stations.csv")
    if (bufferedSource == null) {
      fail("Cannot find the file")
    } else {
      assert(bufferedSource.getLines().next() == "007005,,,")
    }
  }

  test("Is the station hashmap well formed") {
    val hashmap = Extraction.hashStationFile(Extraction.openStationFile("/stations.csv"))
    assert(hashmap.get(("702031","26620"))._1 == 64.7)
    assert(hashmap.get(("702031","26620"))._2 == -162.05)
  }
}