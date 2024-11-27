import scala.io.Source
import scala.util.{Try, Using}

object main{
  class hospitalDataService(data: List[hospitalData]){

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
}