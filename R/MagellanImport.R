#'Import datafiles with Tecan Magellan format
#'
#'used to import data from magellan, please see example in "Examples". Imports raw data from a .csv file. This function is used within "Import"
#'
#'@param in.filename filename, including path, of file you'd like to import.
#'@export
MagellanImport <- function(in.filename){
  table1 <- read.table(paste("Plate_reader_data",in.filename, sep="/"), sep="\t")
  i <- which(grepl("Raw data", table1[,1], useBytes = TRUE))
  table.subset1 <- as.data.frame(table1[(i+1):nrow(table1),])

  j <- which(grepl("Date of measurement", table.subset1[,1]))
  table.subset2 <- as.data.frame(table.subset1[1:(j-1),])
  base.name <- tools::file_path_sans_ext(in.filename)

  new.folder.name <- paste(getwd(), "Spark_data_edited/", sep="/")
  filename.out <- paste(new.folder.name,base.name,"_edited.csv", sep="")

  Cycle_Nr_column <- as.data.frame(matrix(ncol=1, nrow=nrow(table.subset2)))
  Cycle_Nr_column[1,1] <- "Cycle Nr."
  Cycle_Nr_column[2:nrow(Cycle_Nr_column),1] <- 1:(nrow(Cycle_Nr_column)-1)
  table.subset2$`table.subset1[1:(j - 1), ]`<- paste(Cycle_Nr_column$V1, table.subset2$`table.subset1[1:(j - 1), ]`, sep=",")
  write.table(table.subset2, filename.out, col.names=FALSE, row.names=FALSE, quote=FALSE)
}
