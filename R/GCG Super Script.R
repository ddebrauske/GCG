
#function that parses through 96well plate layouts matrices in general
source("ParsePlateLaytout.R")

#function that parses through "plate layout" format from template and makes long table.
#Row and column names are conserved "A1" for example. if you need to change these to match plate reader datafiles, you can change them there. 
#requires ParsePlateLayout() 
source("PlateLayout.R")
source("ParsePlateLaytout.R")
#
layout <- PlateLayout("20200610 Growth Curve Generator/Plate_Layout.csv")



source("PlateBlank.R")#Converts matrix style blanks from template into long table
#
blank <- PlateBlank("20200610 Growth Curve Generator/Plate_Blanks.csv")


source("CombineLayoutBlank.R")
layout.blanks <- CombineLayoutBlank(layoutDF = layout, blankDF = blank)

#takes spark-style multi-sheet excel files and parses into seperate datafiles
source("ExcelToCSV.R")


source("MagellanImport.R")
source("Import.R")
#install.packages("tidyr") used for gather()
library(tidyr)
#requires MagellanImport() ##  <-- DD to combine into one function
#requires tidyr
#make if( no blank )
#
Timepoint.data <- Import(path = "./20200610 Growth Curve Generator/Plate_reader_data/", plate.reader.type = "spark", read.interval = 60)


source("TimeseriesLayoutBlank.R")
data.combined <- TimeseriesLayoutBlank(timepoint.df = Timepoint.data, layout.blank.df = layout.blanks)

#converting into growthcurver format
data.combined.gcr <- matrix(data= NA)
data.combined.gcr <- data.combined
data.combined.gcr[, ncol(data.combined.gcr) + 1 ] <- mapply(paste, sep= "%", data.combined.gcr$plate.name , data.combined.gcr$Coordinate)
data.combined.gcr <- data.combined.gcr[, c(ncol(data.combined.gcr),3:(ncol(data.combined.gcr)-1))]
colnames(data.combined.gcr)[1] <- "Plate-Coordinate"
colnames(data.combined.gcr)[2] <- "time"
data.combined.gcr <- data.combined.gcr[1:3]
data.combined.gcr.wide <- tidyr::pivot_wider(data.combined.gcr, values_from = OD600, names_from = "Plate-Coordinate") #requires tidyr
write.table(data.combined.gcr.wide, file= "./growthcurverfile.csv",sep = "\t", row.names = FALSE)#"\t" means tab

#Growthcurver summary

#install.packages("growthcurver")
library(growthcurver)
file_name <- "./growthcurverfile.csv"
d <- read.table(file_name, header=TRUE, sep= "\t", stringsAsFactors = FALSE, check.names = FALSE)

gc_plate <- growthcurver::SummarizeGrowthByPlate(d)

plate_summary <- gc_plate %>% tidyr::separate(col = sample, into = c("plate", "coordinate"), sep= "%" ) #seperate is from tidyr)
plate_summary$Strain <- NA
plate_summary$Condition <- NA
#write for() loop pulling condition and strain from layout.blanks


#copied and edited from combine layout.blank
for(i in 1:nrow(plate_summary)){ #step through layout blanks row by row
  for(j in 1:nrow(layout.blanks)){ #step through blank.out until you find a matching coordinate&&plate#
    summ.coord <- plate_summary$coordinate[i]
    layout.coord <- layout.blanks$Coordinate[j]
    
    summ.plate.number <- as.character(plate_summary$plate[i])
    layout.plate.number <- as.character(layout.blanks$Plate[j])
    
    
    if(layout.coord == summ.coord && layout.plate.number == summ.plate.number){ #if they match, add OD600 value to the new column of row i
      plate_summary$Strain[i] <- layout.blanks$Strain[j]
      plate_summary$Condition[i] <- layout.blanks$Condition[j]
    }
  }
}






