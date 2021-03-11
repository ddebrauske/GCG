#'Convert tidy data into wide format for growthcurver
#'
#'This function combines the plate name and coordinates from data.combined to one character string separated by a "%". "A1%A1" for example. Therefore, plate name and/or coordinate numbers should not contain "%". output is a .csv file in current directory.
#'@param data.combined the data.combined dataframe specifically from TimeseriesLayoutBlank()
#'@export
Growthcurver_convert <- function(data.combined){

  data.combined.gcr <- matrix(data= NA)
  data.combined.gcr <- data.combined

  data.combined.gcr[, ncol(data.combined.gcr) + 1 ] <- mapply(paste, sep= "@", data.combined.gcr$plate.name , data.combined.gcr$Coordinate)
  data.combined.gcr <- data.combined.gcr[, c(ncol(data.combined.gcr),3:(ncol(data.combined.gcr)-1))]
  colnames(data.combined.gcr)[1] <- "Plate-Coordinate"
  colnames(data.combined.gcr)[2] <- "time"
  data.combined.gcr <- data.combined.gcr[1:3]
  data.combined.gcr.wide <- tidyr::pivot_wider(data.combined.gcr, values_from = OD600, names_from = "Plate-Coordinate") #requires tidyr
  write.table(data.combined.gcr.wide, file= "./growthcurverfile.csv",sep = "\t", row.names = FALSE)#"\t" means tab  this is writing a file with the new growthcurver format

}

  #this is all using growthcurver to generate a summary


#   file_name <- "./growthcurverfile.csv"
#   d <- read.table(file_name, header=TRUE, sep= "\t", stringsAsFactors = FALSE, check.names = FALSE) #takes .csv specified from working directory and imports it into R under the requirements for growthcurver
#
#   gc_plate <- growthcurver::SummarizeGrowthByPlate(d)
#
#   plate_summary <- gc_plate %>% tidyr::separate(col = sample, into = c("plate", "coordinate"), sep= "%" ) #seperate is from tidyr)
#   plate_summary$Strain <- NA
#   plate_summary$Condition <- NA
#   #write for() loop pulling condition and strain from layout.blanks
#
#
# #copied and edited from combine layout.blank
#   for(i in 1:nrow(plate_summary)){ #step through layout blanks row by row
#     for(j in 1:nrow(layout.blanks)){ #step through blank.out until you find a matching coordinate&&plate#
#       summ.coord <- plate_summary$coordinate[i]
#       layout.coord <- layout.blanks$Coordinate[j]
#
#       summ.plate.number <- as.character(plate_summary$plate[i])
#       layout.plate.number <- as.character(layout.blanks$Plate[j])
#
#
#       if(layout.coord == summ.coord && layout.plate.number == summ.plate.number){ #if they match, add OD600 value to the new column of row i
#         plate_summary$Strain[i] <- layout.blanks$Strain[j]
#         plate_summary$Condition[i] <- layout.blanks$Condition[j]
#       }
#     }
#   }



