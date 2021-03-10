#'Parses through plate layout template file
#'
#'A function used to parse through a matrix style plate layouts and create a tidy, long dataframe of the contained information. plate coordinates are read directly from the plate matrix, if you need to change these to match the plate reader datafile output, you can change them no problem. You may copy and paste the plate template as many times as you like. Separate plates are identified in the cell adjcent to "PLATE" in the template. do not change the "PLATE" cell, the script uses this text to find each copied template input. label your plates exactly the same as your plate.reader datafiles and exactly the same as their lables in the similar "plate layout" file. you should have the same ammout of plates in each of these 3 locations.
#'@param path the path, inlcuding filename, to your plate.layout.csv file.
#'@export
PlateLayout <- function(path){

  #path is the path to your plate layout CSV


plate.in <- read.csv(path, header =FALSE)
plate.layout.out <- matrix(nrow=0, ncol=6, data=NA)
plate.layout.out <- as.data.frame(plate.layout.out)

table.temp <- matrix(nrow = 9, ncol=13)
strain.long.temp <- matrix(nrow=0, ncol=4, data=NA)

long.table.temp <- matrix(nrow=96, ncol=7)
long.table.temp <- as.data.frame(long.table.temp)
colnames(long.table.temp) <- c("Plate","Y","X","Condition", "Strain","Coordinate", "Bio Rep")

for(i in 1:nrow(plate.in)){ #pulling apart plate layout input into individual dataframes, then reformatting and combining them together
  if (factor(plate.in[i,1]) == "PLATE"){ #search for the word "PLATE"

    #Condition
    table.temp <- as.data.frame((plate.in)[(i+1):(i+9),1:13]) #Extract 96well matrices from input sheet
    long.table.temp$Plate <- plate.in[i,2] #Plate label
    long.table.temp[,2:4] <- ParsePlateLayout(table.temp) #make 96well format "long"


    #Strain
    table.temp <- as.data.frame((plate.in)[(i+11):(i+19),1:13]) #Extract 96well matrices from input sheet
    strain.long.temp<- ParsePlateLayout(table.temp) #make 96well format "long" to grab "strain" column
    long.table.temp$Strain <- strain.long.temp$Content

    #Bio Rep
    table.temp <- as.data.frame((plate.in)[(i+21):(i+29),1:13]) #Extract 96well matrices from input sheet
    strain.long.temp<- ParsePlateLayout(table.temp) #make 96well format "long" to grab "strain" column
    long.table.temp$`Bio Rep` <- strain.long.temp$Content

    plate.layout.out <- rbind(plate.layout.out, long.table.temp) #add long data to plate.layout.out
  }
}

plate.layout.out$Coordinate <- paste(plate.layout.out[,2], plate.layout.out[,3], sep= "") #combining X and Y coordinates

outDF <- plate.layout.out

return(outDF)

}


