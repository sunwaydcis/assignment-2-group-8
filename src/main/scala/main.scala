import scala.io.Source
import scala.util.Using

class hospitalDataService[T](data: List[T]){

  //1. find state with high total hospital bed?
  def stateWithMaxBeds(implicit ev: T => hospitalData): (String, Int) = {
    val hospitalData = data.map(ev)
    hospitalData.groupBy(_.state)
      .view.mapValues(_.map(_.beds).sum)
      .toMap
      .maxBy(_._2)
  }

  //2. calculate the ratio
  def covidBedRatio(implicit ev: T => hospitalData): Double = {
    val hospitalData = data.map(ev)
    val totalBeds = hospitalData.map(_.beds).sum
    val totalCovidBeds = hospitalData.map(_.bedsCovid).sum
    totalCovidBeds.toDouble / totalBeds
  }

  //3. Calculate the average number of individuals admitted
  def avgAdmissionByState(implicit ev: T => hospitalData): Map[String, (Double, Double, Double)] ={
    val hospitalData = data.map(ev)
    hospitalData.groupBy(_.state).map {
      case (state, records) =>
        val totalRecords = records.size
        val avgPui = records.map(_.admittedPui).sum.toDouble / totalRecords
        val avgCovid = records.map(_.admittedCovid).sum.toDouble / totalRecords
        val avgNonCovid = records.map(_.admittedTotal - _.admittedCovid - _.admittedPui).sum.toDouble / totalRecords
        (state, (avgPui, avgCovid, avgNonCovid))
    }
  }
}

//Function to read CSV file from resources
def readData[T](fileName:String, parse: String => T): List[T] = {
  Using(Source.fromResource(fileName)){
    source => source.getLines().drop(i).map(parse).toList
  }.getOrElse{
    println("Error read the file")
    List.empty[T]
  }
}


object main{
  implicit def csvToHospitalData(line: String): hospitalData = {
    val cols = line.split(",")
    hospitalData(
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
      hospNonCovid = cols(13).toInt,
    )
  }
  
  def main(args: Array[String]): Unit = {
    val hospitalData = readData("hospital.csv", csvToHospitalData)
    
    if(hospitalData.nonEmpty){
      val service = new HospitalDataService[hospitalData](hospitalData)
      
      val (state,maxBeds) = service.stateWithMaxBeds
      println(s"State with the most hospital beds: $state, Total Beds: $maxBeds")
      
      val ratio = service.covidBedRatio
      println(f"Ratio of Covid-19 dedicated beds to total beds: $ratio%.2f")
      
      val averages = service.avgAdmissionByState
      println("Average admission per category per state:")
      averages.foreach{
        case(state, (avgPui, avgCovid, avgNonCovid)) =>
          println(f"State: $state, Suspected: $avgPui%.2f, COVID-19: $avgCovid%.2f, Non-Covid: $avgNonCovid%.2f")
      }
    } else{
      println("No data available to process")
    }
  }
}