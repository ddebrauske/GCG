#'Super! Area Under The Curve -- AUC summary of many curves
#'
#'takes combined data from TimeseriesLayoutBlank, or data with same format and finds AUC for each curve. Requires my eAUC function!
#'
#'
#'@param combined.data a tidy, long dataframe of timepoint,blank, and layout information -- AKA GCG::TimeseriesLayoutBlank function output.
#'@param plot would you like to plot the results with ggplot? plots will include biological replicate points plotted with mean and SE. technical reps -identically identified conditiosn within the same "Bio Rep" will be averaged. defaults to FALSE.
#'@return returns a table of AUC values linked to the corresponding wells
#'@export
SuperAUC <- function(combined.data, plot=FALSE){
data.combined.wide <- matrix(NA)
data.combined.wide <- combined.data

data.combined.wide[, ncol(data.combined.wide) + 1 ] <- mapply(paste, sep= "@", data.combined.wide$Strain , data.combined.wide$Condition, data.combined.wide$Bio_Rep,data.combined.wide$plate.name, data.combined.wide$Coordinate)
colnames(data.combined.wide)[8] <- "Strain.Cond.Rep.Plate.Coord"
colnames(data.combined.wide)[3] <- "time"

data.combined.wide <- data.combined.wide[c(3,4,8)]

data.combined.wide <- as.data.frame(tidyr::pivot_wider(data.combined.wide, values_from = OD600, names_from = "Strain.Cond.Rep.Plate.Coord"))#separate data combined into wide table

data.combined.wide$time <- data.combined.wide$time/60 #to make minutes hours

#for testing out function with individual time series

# time <- data.combined.wide$time
# y <- data.combined.wide$`BY4741@SynH4.2@A@2A.2@B2`
# 
# eAUC <- eAUC(time,y)


eAUC_out <- as.data.frame(matrix(NA, 0,2))
colnames(eAUC_out) <- c("ID", "eAUC")

names <- colnames(data.combined.wide)
for( i in 1:length(names)){
  if(names[i] != "time"){
    
    # x <- data.combined.wide$time
    # n <- length(x)
    # y <- data.combined.wide[,i]
    # 
    eAUC.i <- eAUC(data.combined.wide$time, data.combined.wide[,i])
    ID <- names[i]
    print(paste(i,ID))
    # eAUC.i <- sum((x[2:n] - x[1:n-1]) * (y[2:n] + y[1:n-1]) /  2)
    data.i <- c(ID,eAUC.i)
    eAUC_out <- rbind(eAUC_out,data.i)
  }  
}    
colnames(eAUC_out) <- c("ID", "eAUC")
eAUC_out$eAUC <- as.numeric(eAUC_out$eAUC)

eAUC_out <- tidyr::separate(data=eAUC_out,col = ID, into = c("Strain", "Condition", "Bio_Rep", "Plate", "Coord"), sep= "@" )



if(plot == TRUE){

eAUC_techreps <- plyr::ddply(eAUC_out, c("Strain", "Condition", "Bio_Rep"), plyr::summarise,
                             N    = sum(!is.na(eAUC)),
                             mean = mean(eAUC,na.rm=TRUE),
                             sd   = sd(eAUC,na.rm=TRUE),
                             se   = sd / sqrt(N))

eAUC_techreps <-  dplyr::rename("eAUC" = "mean", .data =  eAUC_techreps)


#~~~~~~~~~~~~~~~~
#Plot Emperical AUC
p<- ggplot2::ggplot(eAUC_techreps, ggplot2::aes(x=Strain, y=eAUC, group=Strain, colour=Strain))+
  ggplot2::facet_wrap(~Condition)+
  ggplot2::geom_point(size= 3)+
  ggplot2::stat_summary(fun = mean, geom="point", shape=15, color="black", fill="black")+
  ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom="errorbar", color= "black", width= 0.3)+
  ggplot2::theme(axis.text.x= ggplot2::element_blank())+
  ggplot2::labs(title= "Emperical AUC",
                x="Strain",
                y="AUC",
                ggplot2::element_text(size=15, face="bold"))
print(p)
}
return(eAUC_out)
}
 
