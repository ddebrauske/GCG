#'Import plate reader data
#'
#'Parse through multiple plate reader data files and combine into large dataframe.plate reader data files must all be in a folder with no other .csv files. i recommend making a new directory for these files. "Plate reader data" would be a good name for it
#'
#'See example formats on github: ddebrauske/GCG
#'
#'@param read.interval time interval, in minutes used in your plate experiment. reads taken every hour = 60
#'@param plate.reader.type plate reader type. must be exactly "spark" or "magellan"
#'@param path path to folder containing plate.csv files
#'@export
Import <- function(path, plate.reader.type, read.interval){

  if(FALSE == dir.exists(paste(path, "Plate_data_edited", sep="/"))){
    dir.create(paste(path, "Plate_data_edited", sep="/")) #this will be a new folder containing reformatted plate reader data#
  }

  if((TRUE %in% grepl(".csv",list.files(path=path))) == FALSE){ #check to make sure there is actually CSVs in your dir.
   stop("Warning: No CSV detected. please check file type")
  }
  
  
  files <- list.files(path=path, pattern="*.csv")
  

  TP.data.long <- as.data.frame(matrix(ncol=4, nrow=0, data=NA)) #TP.data.long will be the timepoint dataframe with all timepoints from all plates

  for(file.i in 1:length(files)){

    print(files[file.i])


    if(plate.reader.type == "spark"){
      #Spark.import(files[i])


        ##Spark import function
        table1 <- read.csv(paste(path, files[file.i], sep="/"))
        # table1 <- read.table(paste(path, files[file.i], sep=""), sep="\t")
        
        cycle.nr.ct <- length(which(grepl("Cycle Nr.", table1[,1], useBytes = TRUE)))
        if(cycle.nr.ct==0|cycle.nr.ct>1){ #check to make sure the format is correct
          stop(paste("Check input format for file:", files[file.i], "Cycle Nr appears more or less than once", sep=" "))
        }
        
        end.time.ct <- length(which(grepl("End Time", table1[,1], useBytes = TRUE)))
        if(end.time.ct==0|end.time.ct>1){ #check to make sure the format is correct
          stop(paste("Check input format for file:", files[file.i], "'End Time' appears more or less than once", sep=" "))
        }
        
        i <- which(grepl("Cycle Nr.", table1[,1], useBytes = TRUE))
        table.subset1 <- as.data.frame(table1[i:nrow(table1),])

        j <- which(grepl("End Time", table.subset1[,1], useBytes= TRUE))
        table.subset2 <- as.data.frame(table.subset1[1:(j-2),])
        base.name <- tools::file_path_sans_ext(files[file.i])

        new.folder.name <- paste(path, "Plate_data_edited/", sep="/")
        filename.out <- paste(new.folder.name,base.name,"_edited.csv", sep="")
        write.table(table.subset2, filename.out, row.names=FALSE,quote = FALSE,col.names = FALSE, sep=",")

        #converts spark machine datasheet into meaningful format. This adds reformatted datafiles to new folder
        Table.i<- read.csv(paste(path, "/Plate_data_edited/", tools::file_path_sans_ext(files[file.i]), "_edited.csv", sep=""))

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

      #MagellanImport <- function(in.filename, out.dir){
        table1 <- read.csv(paste(path, files[file.i], sep=""))
    
        
        if((grepl("Raw data", table1[,1], useBytes = TRUE)[2])== FALSE){ #check to make sure the format is correct
          stop(paste("Check input format for file:", files[file.i], sep=" "))
        }
        
        i <- which(grepl("Raw data", table1[,1], useBytes = TRUE))

        table.subset1 <- as.data.frame(table1[(i+1):nrow(table1),])

        j <- which(grepl("Date of measurement", table.subset1[,1]))
        table.subset2 <- as.data.frame(table.subset1[1:(j-1),])
        base.name <- tools::file_path_sans_ext(files[file.i])

        new.folder.name <- paste(path, "Plate_data_edited/", sep="/")
        filename.out <- paste(new.folder.name,base.name,"_edited.csv", sep="")

        Cycle_Nr_column <- as.data.frame(matrix(ncol=1, nrow=nrow(table.subset2)))
        Cycle_Nr_column[1,1] <- "Cycle Nr."
        Cycle_Nr_column[2:nrow(Cycle_Nr_column),1] <- 1:(nrow(Cycle_Nr_column)-1)
        table.subset2$`table.subset1[1:(j - 1), ]`<- paste(Cycle_Nr_column$V1, table.subset2$`table.subset1[1:(j - 1), ]`, sep=",")
        write.table(table.subset2, filename.out, col.names=FALSE, row.names=FALSE, quote=FALSE, sep=",")

      #Magellan_import(files[i])
      Table.i <- read.csv(paste(path, "/Plate_data_edited/", tools::file_path_sans_ext(files[file.i]), "_edited.csv", sep=""))
      Timepoints <- as.numeric(Table.i[, 1])
      Timepoints <- (Timepoints-1) * read.interval
      Table.i <- Table.i[,c(-1,-2,-3)]

  }else if(plate.reader.type == "sunrise"){

    #MagellanImport <- function(in.filename, out.dir){
    table1 <- read.csv(paste(path, files[file.i], sep=""))
    
    
    if(grepl("Measurement data", table1[,1], useBytes = TRUE)[2] == FALSE){ #check to make sure the format is correct
      stop(paste("Check input format for file:", files[file.i], sep=" "))
    }
    
    i <- which(grepl("Measurement data", table1[,1], useBytes = TRUE))

    table.subset1 <- as.data.frame(table1[(i+1):nrow(table1),])

    j <- which(grepl("Date of measurement", table.subset1[,1]))
    table.subset2 <- as.data.frame(table.subset1[1:(j-1),])
    base.name <- tools::file_path_sans_ext(files[file.i])

    new.folder.name <- paste(path, "Plate_data_edited/", sep="/")
    filename.out <- paste(new.folder.name,base.name,"_edited.csv", sep="")

    Cycle_Nr_column <- as.data.frame(matrix(ncol=1, nrow=nrow(table.subset2)))
    Cycle_Nr_column[1,1] <- "Cycle Nr."
    Cycle_Nr_column[2:nrow(Cycle_Nr_column),1] <- 1:(nrow(Cycle_Nr_column)-1)
    table.subset2$`table.subset1[1:(j - 1), ]`<- paste(Cycle_Nr_column$V1, table.subset2$`table.subset1[1:(j - 1), ]`, sep=",")
    write.table(table.subset2, filename.out, col.names=FALSE, row.names=FALSE, quote=FALSE, sep=",")

    Table.i <- read.csv(paste(path, "/Plate_data_edited/", tools::file_path_sans_ext(files[file.i]), "_edited.csv", sep=""))
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
    Table.i.gathered <- tidyr::gather(Table.i, Timepoint, OD600, 3:ncol(Table.i))
    TP.data.long <- rbind(TP.data.long, Table.i.gathered)
  }
  return(TP.data.long)
}

