#'Plot growth curves by condition
#'
#'Description
#'
#'@param data.combined.summarized Summarized data from ReplicateSummary
#'@param graphic.title general title for all conditions. individual condition name will be appended.
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_conds <- function(data.combined.summarized, graphic.title, path){
  if(FALSE == (dir.exists(paste(path, "Figures", sep="/")))){
    dir.create((paste(path, "Figures", sep="/")))
  }
  if(FALSE == (dir.exists(paste(path, "Figures","SVGs", sep="/")))){
    dir.create((paste(path, "Figures", "SVGs", sep="/")))
  }

  print("generating graphics, please be patient")


  for(cond in unique(data.combined.summarized$Condition)){
    sub1 <- subset(data.combined.summarized, Condition == cond)

    p <- ggplot2::ggplot(data.combined.summarized, ggplot2::aes(x=Time, y=OD600, group=Strain, colour=Strain))+
      ggplot2::geom_errorbar(ggplot2::aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
      ggplot2::geom_line(size=3)+
      ggplot2:: theme(legend.title =ggplot2::element_text(size = 25, face="bold" ),
                      legend.text= ggplot2::element_text(size=20, face="bold"),
                      title=ggplot2::element_text(size= 25, face= "bold"),
                      axis.text.x = ggplot2::element_text(size=16),
                      axis.text.y = ggplot2::element_text(size=16),
                      axis.ticks.length = ggplot2::unit(0.3, "cm"))+
      ggplot2::labs(title= paste(graphic.title, cond, sep="+"),
                    x="Time(h)",
                    y="Cell Density (OD600)",
                    ggplot2::element_text(size=15, face="bold"))+
      ggplot2::scale_x_continuous(breaks = scales::extended_breaks(n=10))



    ggplot2::ggsave(paste((if(FALSE %in% grepl("%", cond)){cond}else{sub( "%", " percent",cond)}), "jpeg", sep="."), path=paste(path, "Figures", sep=""), width = 13, height= 8, device="jpeg", plot = p )

    ggplot2::ggsave(paste(cond, "svg", sep="."), path=paste(path, "Figures/", "SVGs", sep=""), width = 13, height= 8, plot = p)

    print("if error says 'Removed n rows containing missing values (geom_errorbar)', and you have only 1 replicate per condition, please ignore this error")

  }
}


#'Plot summary of all conditions
#'
#'Uses ggplot2's facet_wrap function to plot all conditions and saves as jpeg and svg. This
#'
#'@param data.combined.summarized tidy, long data from ReplicateSummary
#'@param graphic.title what you would like to title this graphic
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_wrap <- function(data.combined.summarized, graphic.title, path){

  if(FALSE == (dir.exists(paste(path, "Figures", sep="/")))){
    dir.create((paste(path, "Figures", sep="/")))
  }
  if(FALSE == (dir.exists(paste(path, "Figures","SVGs", sep="/")))){
    dir.create((paste(path, "Figures", "SVGs", sep="/")))
  }

  p <- ggplot2::ggplot(data.combined.summarized, ggplot2::aes(x=Time, y=OD600, group=Strain, colour=Strain))+
    ggplot2::facet_wrap(~Condition)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
    ggplot2::geom_line(size=2)+
    ggplot2:: theme(legend.title =ggplot2::element_text(size = 15, face="bold" ),
          legend.text= ggplot2::element_text(size=15, face="bold"),
          title=ggplot2::element_text(size= 20, face= "bold"),
          strip.text.x =  ggplot2::element_text(size=12),
          axis.ticks.length = ggplot2::unit(0.3, "cm"))+
    ggplot2::labs(title= graphic.title,
         x="Time(h)",
         y="Cell Density (OD600)",
         ggplot2::element_text(size=15, face="bold"))+
    ggplot2::scale_x_continuous(breaks = scales::extended_breaks(n=10))

  ggplot2::ggsave("Facet_Wrap.jpeg", path = paste(path, "Figures", sep=""), width = 13, height= 8, device="jpeg", plot = p)
  #ggplot2::ggsave(paste("Facet_Wrap.svg"), path = paste(path, "Figures/", "SVGs", sep=""),width = 13, height= 8, plot = p)

  print("check wd for new 'Figures' folder, containing generated graphics")
  print("if error says 'Removed n rows containing missing values (geom_errorbar)', and you have only 1 replicate per condition, please ignore this error")

  return(p)
}


#'Wrap individual biological replicates to spot-check
#'
#'Subsets each biological replicate and plots a facet_wrap with every condition. this allows you to spot check the biological reps to see if there is any obvious problems.
#'
#'@param data.combined tidy, long data from TimeseriesLayoutBlank function
#'@param graphic.title what you would like to title this graphic
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_bioreps <- function(data.combined, graphic.title, path){

  bio.reps.list <- unique(data.combined$Bio_Rep)

  for(i in 1:length(bio.reps.list)){

      data.combined.summarized.no.empty.brep <- subset(data.combined, Bio_Rep == bio.reps.list[i])
      colnames(data.combined.summarized.no.empty.brep)[3] <- "Time"

      data.combined.summarized.tidy.brep <- plyr::ddply(data.combined.summarized.no.empty.brep, c("Strain", "Condition", "Time"), plyr::summarise,
                                       N    = sum(!is.na(OD600)),
                                       mean = mean(OD600,na.rm=TRUE),
                                       sd   = sd(OD600,na.rm=TRUE),
                                       se   = sd / sqrt(N))

      tech.rep.only <- "Showing only technical replicates"

      data.combined.summarized.tidy.brep<- dplyr::rename("OD600" = "mean", .data = data.combined.summarized.tidy.brep )
      data.combined.summarized.tidy.brep$Time <- as.numeric(as.character(data.combined.summarized.tidy.brep$Time))
      data.combined.summarized.tidy.brep$Time <- data.combined.summarized.tidy.brep$Time / 60 #make it per hr.
      data.combined.summarized.tidy.brep$Time <- factor(data.combined.summarized.tidy.brep$Time)
      data.combined.summarized.tidy.brep$Condition <- factor(data.combined.summarized.tidy.brep$Condition)
      data.combined.summarized.tidy.brep$OD600 <- as.numeric(data.combined.summarized.tidy.brep$OD600)
      data.combined.summarized.tidy.brep$Time <- as.numeric(as.character(data.combined.summarized.tidy.brep$Time))

     p <- ggplot2::ggplot(data.combined.summarized.tidy.brep, ggplot2::aes(x=Time, y=OD600, group=Strain, colour=Strain)
                          )+
       ggplot2::facet_wrap(~Condition
                           )+
       ggplot2::geom_errorbar(ggplot2::aes(ymin=OD600-se, ymax=OD600+se), width=.1
                              )+
       ggplot2::geom_line(size=2)+
       ggplot2:: theme(legend.title =ggplot2::element_text(size = 15, face="bold"),
                       legend.text= ggplot2::element_text(size=15, face="bold"),
                       title=ggplot2::element_text(size= 20, face= "bold"),
                       strip.text.x =  ggplot2::element_text(size=12),
                       axis.ticks.length = ggplot2::unit(0.3, "cm")
                       )+
       ggplot2::labs(title= paste(graphic.title, "BioRep", bio.reps.list[i]),
                     x="Time(h)",
                     y="Cell Density (OD600)",
                     ggplot2::element_text(size=15, face="bold"))+
       ggplot2::scale_x_continuous(breaks = scales::extended_breaks(n=10))

     print(p)

      ggplot2::ggsave(paste("Facet_Wrap_biorep" ,i, ".jpeg"), path=paste(path, "Figures", sep="/"), width = 13, height= 8, device="jpeg", plot = p  )
      #ggplot2::ggsave(paste("Facet_Wrap_biorep", i, ".svg"), path=paste(wd, "Figures", "SVGs", sep="/"),width = 13, height= 8, plot= p)
  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#creating a matrix of all growth curves (magellan style)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'Display curves from all wells
#'
#'Plots each curve individually, plotting each plate as one facet_wrap matrix, in 96 well format. this allows for manual spot-checking of technical replicates and individual wells.
#'
#'@param data.combined.summarized tidy, long data from SummarizeDataCombined
#'@param graphic.title what you would like to title this graphic
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_matrices <- function(data.combined.summarized, graphic.title, path){
  plate.names <- unique(data.combined.summarized$plate.name)

  for(i in 1:length(plate.names)){

    single.plate.data <- subset(data.combined.summarized, plate.name == plate.names[i])

    single.plate.data$Time <- as.numeric(single.plate.data$Time)
    single.plate.data$Coordinate <- factor(single.plate.data$Coordinate, levels = unique(single.plate.data$Coordinate))

    p <- ggplot2::ggplot(single.plate.data, ggplot2::aes(x=Time, y=OD600, group=Strain, colour=Strain))+
      ggplot2::facet_wrap(~Coordinate, ncol= 10)+
      ggplot2::geom_line(size=2)+
      ggplot2::theme(legend.title =ggplot2::element_text(size = 15, face="bold" ),
                      legend.text= ggplot2::element_text(size=15, face="bold"),
                      title=ggplot2::element_text(size= 20, face= "bold"),
                      strip.text.x =  ggplot2::element_text(size=12),
                      axis.ticks.length = ggplot2::unit(0.3, "cm"))+
      ggplot2::labs(title= paste(graphic.title, "plate", plate.names[i]),
                    x="Time(h)",
                    y="Cell Density (OD600)",
                    ggplot2::element_text(size=15, face="bold"),
                    caption = "showing only technical replicates")+
      ggplot2::scale_x_continuous(breaks = scales::extended_breaks(n=5))

    print(p)

    ggplot2::ggsave(paste("plate matrix", plate.names[i], ".jpeg"), path=paste(path, "Figures", sep="/"),width = 13, height= 8, plot = p)

  }
}



#deplyr plyr scales"
