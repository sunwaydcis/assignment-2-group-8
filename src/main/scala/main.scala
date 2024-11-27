import scala.io.Source
import scala.util.Using

object main{
  private class hospitalDataService(data: List[hospitalData]){

    //1. find state with high total hospital bed?
    def stateWithMaxBeds: (String, Int) = {
      data.groupBy(_.state)
        .view.mapValues(_.map(_.beds).sum)
        .toMap
        .maxBy(_._2)
    }

    //2. calculate the ratio
    def covidBedRatio: Double = {
      val totalBeds = data.map(_.beds).sum
      val totalCovidBeds = data.map(_.bedsCovid).sum
      totalCovidBeds.toDouble / totalBeds
    }

    //3. Calculate the average number of individuals admitted
    def avgAdmissionByState: Map[String, (Double, Double, Double)] = {
      data.groupBy(_.state).map {case(state, records) =>
        val totalRecords = records.size
        val avgPui = records.map(_.admittedPui).sum.toDouble/ totalRecords
        val avgCovid = records.map(_.admittedCovid).sum.toDouble/totalRecords
        val avgNonCovid = records.map(_.admittedNonCovid).sum.toDouble/ totalRecords
        (state, (avgPui, avgCovid, avgNonCovid))
      }
    }
  }

  //Function to read CSV file from resources
  private def readData(fileName: String): List[hospitalData]={
    Using(Source.fromResource(fileName)){ source =>
      source.getLines().drop(1).map{ line =>
        val cols = line.split(",")
        hospitalData(
          date = cols(0),
          state = cols(1),
          beds = cols(2).toInt,
          bedsCovid = cols(3).toInt,
          admittedPui = cols(4).toInt,
          admittedCovid = cols(5).toInt,
          admittedNonCovid = cols(6).toInt,
        )
      }.toList
    }.getOrElse{
      println("Error reading the file. Please check & try again")
      List.empty
    }
  }

  def main(args: Array[String]): Unit = {
    //Read the hospital data from the CSV file located in resources
    val hospitalData = readData("hospital.csv")

    //proceed once data is loaded
    if(hospitalData.nonEmpty){
      val service = new hospitalDataService(hospitalData)

      val (state, maxBeds) = service.stateWithMaxBeds
      println(s"State with the highest total hospital beds: $state, Total Beds: $maxBeds")

      val ratio = service.covidBedRatio
      println(f"Ratio of COVID-19 dedicated beds to total beds: $ratio%.2f")

      val averages = service.avgAdmissionByState
      println("Average admissions per category per state:")
      averages.foreach{ case(state, (avgPui, avgCovid, avgNonCovid)) =>
      println(f"State: $state, Suspected: $avgPui%.2f, COVID-19: $avgCovid%.2f, Non-COVID: $avgNonCovid%.2f")
      }
    } else {
      println("No data available to process")
    }
  }
}