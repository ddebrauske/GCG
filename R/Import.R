#`Parse through multiple plate reader data files and combine into large dataframe.plate reader data files must all be in a folder with no other .csv files. i recommend making a new directory for these files. "Plate reader data" would be a good name for it
#`
#``
#`@param read.interval time interval, in minutes used in your plate experiment. reads taken every hour = 60
#`@param plate.reader.type plate reader type. must be exactly "spark" or "magellan"
#`@param path path to folder containing plate.csv files
#`@export
Import <- function(path, plate.reader.type, read.interval){

  if(FALSE == dir.exists(paste(dirname(path), "Spark_data_edited", sep="/"))){
    dir.create(paste(dirname(path), "Spark_data_edited", sep="/")) #this will be a new folder containing reformatted plate reader data#
  }


  files <- list.files(path=path, pattern="*.csv")

  TP.data.long <- as.data.frame(matrix(ncol=4, nrow=0, data=NA)) #TP.data.long will be the timepoint dataframe with all timepoints from all plates

  for(file.i in 1:length(files)){

    print(files[file.i])


    if(plate.reader.type == "spark"){
      #Spark.import(files[i])


        ##Spark import function
        table1 <- read.table(paste(path, files[file.i], sep="/"), sep="\t")
        i <- which(grepl("Cycle Nr.", table1[,1], useBytes = TRUE))
        table.subset1 <- as.data.frame(table1[i:nrow(table1),])

        j <- which(grepl("End Time", table.subset1[,1]))
        table.subset2 <- as.data.frame(table.subset1[1:(j-2),])
        base.name <- tools::file_path_sans_ext(files[file.i])

        new.folder.name <- paste(dirname(path), "Spark_data_edited/", sep="/")
        filename.out <- paste(new.folder.name,base.name,"_edited.csv", sep="")
        write.table(table.subset2, filename.out, col.names=FALSE, row.names=FALSE, quote=FALSE)

        #converts spark machine datasheet into meaningful format. This adds reformatted datafiles to new folder
        Table.i<- read.csv(paste(dirname(path), "/Spark_data_edited/", tools::file_path_sans_ext(files[file.i]), "_edited.csv", sep=""))

        #added 20210202 to remove extra rows with no data
        NA.list <- which(is.na(Table.i[1,]))
        if(is.na(NA.list[1]) == FALSE){
          Table.i <- Table.i[,-NA.list]
        }

        for(k in 4:ncol(Table.i)){ #finds non-numeric data point, replaces with NA. this will occur if spectrometer is oversaturated and produces text "OVER" or something of that nature.
          if(sapply(Table.i[k], class) != "numeric"){
            Table.i[k] <- as.numeric(as.character(Table.i[k]))
            print(paste("in row", colnames(Table.i)[k], "non-numeric character found, replaced with NA"))
          }
        }

      Timepoints <- as.numeric(Table.i[, 1])
      Timepoints <- (Timepoints-1) * read.interval
      Table.i <-  Table.i[,c(-1,-2,-3)]#removes time(s) column, temp column
      print(i)
     #20210304 ^^ working on getting spark import to work off a designated file path


    }else if(plate.reader.type == "magellan"){
      Magellan_import(files[i])
      Table.i <- read.csv(paste(dirname(path), "/Spark_data_edited/", tools::file_path_sans_ext(files[i]), "_edited.csv", sep=""))
      Timepoints <- as.numeric(Table.i[, 1])
      Timepoints <- (Timepoints-1) * read.interval
      Table.i <- Table.i[,c(-1,-2,-3)]

    }

    #importing/rearranging input table from machine
    #convert cycle number to actual time interval
    Table.i <- as.data.frame(t(Table.i))#transpose to wide
    Table.i$Coordinate <- row.names(Table.i)
    Table.i$plate.name <- tools::file_path_sans_ext(files[file.i])
    Table.i <- Table.i[,c(ncol(Table.i)-1, ncol(Table.i), 1:(ncol(Table.i)-2))]
    colnames(Table.i)[3:ncol(Table.i)] <- Timepoints
    Table.i.gathered <- gather(Table.i, Timepoint, OD600, 3:ncol(Table.i))
    TP.data.long <- rbind(TP.data.long, Table.i.gathered)
  }
  return(TP.data.long)
}
