package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  test("Can we open station the station file") {
    val bufferedSource = Extraction.openStationFile("/stations.csv")
    if (bufferedSource == null) {
      fail("Cannot find the file")
    } else {
      assert(bufferedSource.getLines.take(0) == ",94234,+49.083,-125.767")
    }
    /*
      for (line <- bufferedSource.getLines) {
        val cols = line.split(",").map(_.trim)
        // do whatever you want with the columns here
        for (col <- cols) {
          print(col + ", ")
        }
        println()
      }
      bufferedSource.close()
    }
  }*/
  }
}