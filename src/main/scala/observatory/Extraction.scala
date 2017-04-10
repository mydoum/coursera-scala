package observatory

import java.time.LocalDate
import java.util

import scala.io.BufferedSource
import java.util.HashMap

import scala.collection.mutable.ListBuffer
import scala.collection.{LinearSeq, mutable}
import scala.math.BigDecimal

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val finalSequence = new ListBuffer[(LocalDate, Location, Double)]()
    val hashStations = hashStationFile(openStationFile(stationsFile))
    val bufferedWeatherFile = openStationFile(temperaturesFile)

    for (line <- bufferedWeatherFile.getLines()) {
      val cols = line.split(",").map(_.trim)
      val location = createLocation(hashStations, cols(0), cols(1))
      if (location != null && cols(4) != "9999.99") {
        val averageDegreeCelcius = fahrenheitToCelsius(cols(4).toDouble)
        finalSequence += ((LocalDate.of(year, cols(2).toInt, cols(3).toInt), location, averageDegreeCelcius))
      }
    }

    finalSequence
  }

  def createLocation(hashmap: util.HashMap[(String, String), (Double, Double)], key_1: String, key_2: String): Location = {
    if (!hashmap.containsKey((key_1, key_2))) {
      null
    } else {
      val StationLocation = hashmap.get((key_1, key_2))
      Location(StationLocation._1, StationLocation._2)
    }
  }

  def openStationFile(file: String): BufferedSource = {
    val bufferedSource = new BufferedSource(getClass.getResourceAsStream(file))
    bufferedSource
  }

  def hashStationFile(bufferedSource: BufferedSource): HashMap[(String, String), (Double, Double)] = {
    val hashmap = new util.HashMap[(String, String), (Double, Double)]()
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      if (cols.length < 4 || cols(2).isEmpty || cols(3).isEmpty) {
      } else {
        // do whatever you want with the columns here
        hashmap.put((cols(0), cols(1)), (cols(2).toDouble, cols(3).toDouble))
      }
      bufferedSource.close()
    }

    hashmap
  }

  def fahrenheitToCelsius(value: Double) : Double = {
    BigDecimal((value - 32) / 1.8).setScale(2, BigDecimal.RoundingMode.HALF_EVEN).toDouble
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val finalSequence = new ListBuffer[(Location, Double)]()
    val it = records.iterator
    if (it.hasNext) {
      var tmp = it.next()
      var average = tmp._3
      var counter = 1
      var location = tmp._2

      while(it.hasNext) {
        tmp = it.next()
        if (tmp._2 == location) {
          average += tmp._3
          counter += 1
        } else {
          finalSequence += ((location, average / counter))
          average = tmp._3
          counter = 1
          location = tmp._2
        }
      }
      finalSequence += ((location, average / counter))
    }

    finalSequence
  }

}
