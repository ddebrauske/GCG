#'Summarize technical replicates
#'
#'Produces mean and SE of technical and biological replicates. if you have multiple biological replicates, this function will first mean the technical replicates within them, then find SE and mean of those means.
#'
#'@param data.combined tidy, long dataframe with columns "Coordinate", "plate.name", "Time", "OD600", "Strain", "Condition", "Bio_rep" -- from TimeseriesLayoutBlank function
#'@return data.combined.tidy a summarized table of your data, including average and SE for every timepoint. this will be used for graphing with ggplot.
#'@export
SummarizeTechReps <- function(data.combined){

  data.combined.tidy <- plyr::ddply(data.combined, c("Strain", "Condition", "Time", "Bio_Rep"), plyr::summarise,
                                    N    = sum(!is.na(OD600)),
                                    mean = mean(OD600,na.rm=TRUE),
                                    sd   = sd(OD600,na.rm=TRUE),
                                    se   = sd / sqrt(N))
  print("Showing only technical replicates")

  data.combined.tidy <-  dplyr::rename("OD600" = "mean", .data =  data.combined.tidy)
  data.combined.tidy$Time <- as.numeric(as.character(data.combined.tidy$Time))
  data.combined.tidy$Time <- data.combined.tidy$Time / 60 #make it per hr.
  data.combined.tidy$Condition <- factor(data.combined.tidy$Condition)
  data.combined.tidy$OD600 <- as.numeric(data.combined.tidy$OD600)


  return(data.combined.tidy)
}


#'Summarize biological replicates
#'
#'Produces mean and SE of technical and biological replicates. if you have multiple biological replicates, this function will first mean the technical replicates within them, then find SE and mean of those means.
#'
#'@param TechRepSummary dataframe produced with TimeseriesLayoutBlank, or similar long table.
#'@return summarized table of your data, including average and SE for every timepoint. this will be used for graphing with ggplot.
#'@export
SummarizeBioReps <- function(TechRepSummary){

    if(length(unique(data.combined$Bio_Rep)) > 1){

      #average tech rep averages (bio replicates) and find SE
      data.combined.tidy <- plyr::ddply(TechRepSummary, c("Strain", "Condition", "Time"), plyr::summarise,
                                        N    = sum(!is.na(OD600)),
                                        mean = mean(OD600,na.rm=TRUE),
                                        sd   = sd(OD600,na.rm=TRUE),
                                        se   = sd / sqrt(N))
    }else{

      #if only 1 biological replicate, average technical replicates only, display error as SE bars.

      stop("Warning: Data has only 1 biological replicate")
    }

    data.combined.tidy <-  dplyr::rename("OD600" = "mean", .data =  data.combined.tidy)
    data.combined.tidy$Time <- as.numeric(as.character(data.combined.tidy$Time))#make it per hr.
    data.combined.tidy$Condition <- factor(data.combined.tidy$Condition)
    data.combined.tidy$OD600 <- as.numeric(data.combined.tidy$OD600)


    return(data.combined.tidy)
  }



