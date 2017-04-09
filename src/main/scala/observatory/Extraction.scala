package observatory

import java.time.LocalDate
import java.util

import scala.io.BufferedSource
import java.util.HashMap

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
    val bufferedSource = openStationFile(stationsFile)
    bufferedSource.close
    return null
  }

  def openStationFile(stationsFile: String): BufferedSource = {
    val bufferedSource = new BufferedSource(getClass.getResourceAsStream(stationsFile))
    return bufferedSource
  }

  def hashStationFile(bufferedSource: BufferedSource): HashMap[(String, String), (Double, Double)] = {
    var hashmap = new util.HashMap[(String, String), (Double, Double)]()
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      if (cols.length < 4 || cols(2).isEmpty || cols(3).isEmpty) {
      } else {
        // do whatever you want with the columns here
        hashmap.put((cols(0), cols(1)), (cols(2).toDouble, cols(3).toDouble))
      }
      bufferedSource.close()
    }
    return hashmap
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

}
