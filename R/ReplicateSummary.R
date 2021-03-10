#'Summarize replicates
#'
#'Produces mean and SE of technical and biological replicates. if you have multiple biological replicates, this function will first mean the technical replicates within them, then find SE and mean of those means. 
#'
#'@param data.combined table produced with TimeseriesLayoutBlank, or similar long table.
#'@return data.combined.tidy a summarized table of your data, including average and SE for every timepoint. this will be used for graphing with ggplot.
#'@export
ReplicateSummary <- function(data.combined){
  
  if(length(unique(data.combined$Bio_Rep)) > 1){
    
    #first average tech replicates,  
    tech.reps.averaged <- dplyr::ddply(data.combined.no.empty, c("Strain", "plate.name", "Condition", "Time", "Bio_Rep"), summarise,
                                N    = sum(!is.na(OD600)),
                                mean = mean(OD600,na.rm=TRUE))
    
    tech.reps.averaged<- tech.reps.averaged %>% dplyr::rename("OD600" = "mean")
    
    #then average those averages (bio replicates) and find SE
    data.combined.tidy <- dplyr::ddply(tech.reps.averaged, c("Strain", "Condition", "Time"), summarise,
                                N    = sum(!is.na(OD600)),
                                mean = mean(OD600,na.rm=TRUE),
                                sd   = sd(OD600,na.rm=TRUE),
                                se   = sd / sqrt(N))
  }else{
    
    #if only 1 biological replicate, average technical replicates only, display error as SE bars. 
    data.combined.tidy <- dplry::ddply(data.combined.no.empty, c("Strain", "Condition", "Time"), summarise,
                                N    = sum(!is.na(OD600)),
                                mean = mean(OD600,na.rm=TRUE),
                                sd   = sd(OD600,na.rm=TRUE),
                                se   = sd / sqrt(N))
    print("Showing only technical replicates")
  }
  
  data.combined.tidy <- data.combined.tidy %>% deplyr::rename("OD600" = "mean")
  data.combined.tidy$Time <- as.numeric(as.character(data.combined.tidy$Time))
  data.combined.tidy$Time <- data.combined.tidy$Time / 60 #make it per hr. 
  data.combined.tidy$Time <- factor(data.combined.tidy$Time) 
  data.combined.tidy$Condition <- factor(data.combined.tidy$Condition)
  data.combined.tidy$OD600 <- as.numeric(data.combined.tidy$OD600)

  return(data.combined.tidy)
}

