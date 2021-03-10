#'Parse through blank readings
#'
#'A function used to parse through a matrix style plate layouts and create a tidy, long dataframe of the contained information. plate coordinates are read directly from the plate matrix, if you need to change these to match the plate reader datafile output, you can change them no problem. You may copy and paste the plate template as many times as you like. Separate plates are identified in the cell adjcent to "PLATE" in the template. do not change the "PLATE" cell, the script uses this text to find each copied template input. label your plates exactly the same as your plate.reader datafiles and exactly the same as their labels in the similar "plate layout" file. you should have the same amount of plates in each of these 3 locations.
#'
#'@param path the path (including filename) to plate.blank datafile, modified from template.
#'@return outDF a tidy, long dataframe with the information provided in your input file.
#'@export
PlateBlank <- function(path){
long.table.temp <- matrix(nrow=96, ncol=5)
long.table.temp <- as.data.frame(long.table.temp)
colnames(long.table.temp) <- c("Plate","Y","X","OD600","Coordinate")

blank.out <- matrix(nrow=0, ncol=5, data=NA)
blank.out <- as.data.frame(blank.out)

blank.in <- read.csv(path, header = FALSE)

for(i in 1:nrow(blank.in)){ #pulling apart blank input and extracting individual dataframes, reformatting and combinging them together
  if (factor(blank.in[i,1]) == "PLATE"){
    print(i)

    table.temp <- as.data.frame((blank.in)[(i+1):(i+9),1:13]) #Extract 96well matrices from input sheet
    long.table.temp$Plate <- blank.in[i,2] #Plate label
    long.table.temp[,2:4] <- ParsePlateLayout(table.temp) #make 96well format "long"

    blank.out <- rbind(blank.out, long.table.temp) #add long data to plate.layout.out
  }
}
blank.out$Coordinate <- paste(blank.out[,2], blank.out[,3], sep= "")

outDF <- blank.out

return(outDF)
}
