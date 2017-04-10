package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate
import scala.collection.mutable.ListBuffer

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

  test("Fahrenheit convertor") {
    assert(Extraction.fahrenheitToCelsius(81.14) == 27.3)
    assert(Extraction.fahrenheitToCelsius(32) == 0)
    assert(Extraction.fahrenheitToCelsius(35.6) == 2.0)
    assert(Extraction.fahrenheitToCelsius(33.8) == 1.0)
    assert(Extraction.fahrenheitToCelsius(122) == 50.0)
  }

  test("LocateTemperatures") {
    val sequence = Extraction.locateTemperatures(2015, "/testStations_1.csv", "/testWeather_1.csv")
    assert(sequence.head._1 == LocalDate.of(2015, 8, 11))
    assert(sequence.head._2 == Location(37.35, -78.433))
    assert(sequence.head._3 == 27.3)
  }

  test("LocateTemperature2") {
    val sequence = Extraction.locateTemperatures(2015, "/testStations_1.csv", "/testWeather_1.csv")
    val result = Seq (
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    assert(sequence == result)
  }

  test("AverageWeatherLocation") {
    val sequence = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/testStations_1.csv", "/testWeather_1.csv"))
    assert(sequence.head._1 == Location(37.35, -78.433))
    assert(sequence.head._2 == 27.3)
  }

  test("AverageWeatherLocation2") {
    val sequence = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/testStations_1.csv", "/testWeather_1.csv"))
    val result = Seq (
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

    assert(sequence == result)
  }
}