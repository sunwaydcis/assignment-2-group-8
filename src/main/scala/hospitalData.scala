case class hospitalData(
                       date:String, //date
                       state:String, //state names
                       beds:Int, //total hosp beds
                       bedsCovid:Int, //total beds for covid19
                       bedsNonCrit:Int, //total hop beds for non crit
                       admittedPui:Int, //admitted suspected cases
                       admittedCovid:Int, //admitted C19+ case
                       admittedTotal:Int, //admitted individuals
                       dischargedPui:Int, // discharged suspected cases
                       dischargedCovid:Int, //discharfed C19+
                       dischargedTotal:Int, //total discharged
                       hospCovid:Int, //total C19+ in hosp
                       hospPui:Int, //total suspected in hosp
                       hospNonCovid:Int //total non-C19 in hosp
                       )