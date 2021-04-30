#'Annotate raw data ~~under construction~~
#' 
#'Step through raw data and add strain and condition information. Each plate exported as a sheet in an Excel file
#' 
#'@param path path to edited plate data. This folder should be automatically created with the GCG::Import function
#'@param layout.blank.df dataframe containing your combined layout and blank information
#'@param filename what would you like to name the output file? please include .xlsx
#'@param out.dir where would you like to save the file?
#'@export
Annotate <- function(path, layout.blank.df, filename, out.dir){
  
#path<- "C:/Users/Derek Debrauske/Dropbox/R/Projects/20210330 GCG superscript testing/20210303 Chemgen validation R2/Plate_data_edited/"
  
#layout.blank.df<- layout.blanks
  
# filename <- "DataSummary.xlsx"

# out.dir <- "C:/Users/Derek Debrauske/Desktop/"

  if(exists("raw.data.summary")){ #remove current raw data summary to avoid appending multiple. 
    rm(raw.data.summary)
  }
  
  raw.data.summary <- openxlsx::createWorkbook()
  
  for(i in 1:length(list.files(path))){
    Summary.i <- read.csv(list.files(path, full.names = TRUE)[i])
    Summary.i <- dplyr::rename(Summary.i, "Timepoint(manual)"="Cycle.Nr.")
    extra_rows <- as.data.frame(matrix(ncol=ncol(Summary.i), nrow=6))
    colnames(extra_rows) <- colnames(Summary.i)
    Summary.i <- rbind(extra_rows, Summary.i)
    rownames(Summary.i)[1:6] <- c("Condition", "Strain", "Plate", "Coordinate", "Blank subtract value", "Biological replicate")
    
    Summary.i["Plate", 4:ncol(Summary.i) ] <- unlist(strsplit((list.files(path)[i]), split ="_"))[1]
    Summary.i["Coordinate", 4:ncol(Summary.i)] <- colnames(Summary.i)[4:ncol(Summary.i)]
    
    for(j in 4:ncol(Summary.i)){
      for(k in 1:nrow(layout.blanks)){
        if(Summary.i["Plate",j] == layout.blanks$Plate[k] 
           && Summary.i["Coordinate", j] == layout.blanks$Coordinate[k]){
          
          Summary.i["Strain", j]  <- as.character(layout.blanks$Strain[k])
          Summary.i["Condition",j] <- as.character(layout.blanks$Condition[k])
          Summary.i["Blank subtract value", j] <- as.character(layout.blanks$OD600[k])
          Summary.i["Biological replicate", j] <- as.character(layout.blanks$`Bio Rep`[k])
        }
      }
    } 
    
    
    
    
    openxlsx::addWorksheet(raw.data.summary, paste("Plate",unlist(strsplit((list.files(path)[i]), split ="_"))[1], sep=" "))
    
    openxlsx::writeData(raw.data.summary, paste("Plate",unlist(strsplit((list.files(path)[i]), split ="_"))[1], sep=" "), Summary.i, rowNames = TRUE)
    
  }
  
  
  openxlsx::saveWorkbook(raw.data.summary, paste(path,filename, sep="/"), overwrite=(file.exists(paste(out.dir, filename, sep="/"))))
  
  openxlsx::openXL(paste(our.dir,filename, sep="/"))
  
}#function 
print(":-)")
