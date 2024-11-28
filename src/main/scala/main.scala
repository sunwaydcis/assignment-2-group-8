// main.scala
import scala.io.Source
import scala.language.implicitConversions
import scala.util.Using

// Generic class to handle hospital data, now works with any type T
class HospitalDataService[T](data: List[T]) {

  // Find the state with the highest total hospital beds
  def stateWithMaxBeds(implicit ev: T => HospitalData): (String, Int) = {
    val hospitalData = data.map(ev)  // Convert T to HospitalData
    hospitalData.groupBy(_.state)
      .view.mapValues(_.map(_.beds).sum)
      .toMap
      .maxBy(_._2)
  }

  // Calculate the ratio of COVID beds to total hospital beds
  def covidBedRatio(implicit ev: T => HospitalData): Double = {
    val hospitalData = data.map(ev)  // Convert T to HospitalData
    val totalBeds = hospitalData.map(_.beds).sum
    val totalCovidBeds = hospitalData.map(_.bedsCovid).sum
    totalCovidBeds.toDouble / totalBeds
  }

  // Calculate the average number of individuals admitted in each category (PUI, COVID, non-COVID) by state
  def avgAdmissionByState(implicit ev: T => HospitalData): Map[String, (Double, Double, Double)] = {
    val hospitalData = data.map(ev)  // Convert T to HospitalData
    hospitalData.groupBy(_.state).map { case (state, records) =>
      val totalRecords = records.size
      val avgPui = records.map(_.admittedPui).sum.toDouble / totalRecords
      val avgCovid = records.map(_.admittedCovid).sum.toDouble / totalRecords
      val avgNonCovid = records.map(record => record.admittedTotal - record.admittedCovid - record.admittedPui).sum.toDouble / totalRecords
      (state, (avgPui, avgCovid, avgNonCovid))
    }
  }
}

// Function to read CSV file and parse data into a List of T
def readData[T](fileName: String, parse: String => T): List[T] = {
  Using(Source.fromResource(fileName)) { source =>
    source.getLines().drop(1).map(parse).toList
  }.getOrElse {
    println("Error reading the file. Please check & try again.")
    List.empty[T]
  }
}

// Main object to run the program
object Main {

  // Implicit function to convert a CSV line to HospitalData
  implicit def csvToHospitalData(line: String): HospitalData = {
    val cols = line.split(",")
    HospitalData(
      date = cols(0),
      state = cols(1),
      beds = cols(2).toInt,
      bedsCovid = cols(3).toInt,
      bedsNonCrit = cols(4).toInt,
      admittedPui = cols(5).toInt,
      admittedCovid = cols(6).toInt,
      admittedTotal = cols(7).toInt,
      dischargedPui = cols(8).toInt,
      dischargedCovid = cols(9).toInt,
      dischargedTotal = cols(10).toInt,
      hospCovid = cols(11).toInt,
      hospPui = cols(12).toInt,
      hospNonCovid = cols(13).toInt
    )
  }

  def main(args: Array[String]): Unit = {
    // Read the CSV file and parse it into a List[HospitalData]
    val hospitalData = readData("hospital.csv", csvToHospitalData)

    // Proceed once data is loaded
    if (hospitalData.nonEmpty) {
      // Create a HospitalDataService to process the data
      val service = new HospitalDataService[HospitalData](hospitalData)

      // 1. Find the state with the highest total hospital beds
      val (state, maxBeds) = service.stateWithMaxBeds
      println(s"State with the highest total hospital beds: $state, Total Beds: $maxBeds")

      // 2. Calculate the ratio of COVID beds to total hospital beds
      val ratio = service.covidBedRatio
      println(f"Ratio of COVID-19 dedicated beds to total beds: $ratio%.2f")

      // 3. Calculate average number of admissions per category (PUI, COVID, non-COVID) per state
      val averages = service.avgAdmissionByState
      println("Average admissions per category per state:")
      averages.foreach { case (state, (avgPui, avgCovid, avgNonCovid)) =>
        println(f"State: $state, Suspected: $avgPui%.2f, COVID-19: $avgCovid%.2f, Non-COVID: $avgNonCovid%.2f")
      }
    } else {
      println("No data available to process")
    }
  }
}