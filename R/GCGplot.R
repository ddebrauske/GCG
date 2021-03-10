#'Plot growth curves by condition
#'
#'Description
#'
#'@param data.combined.summarized Summarized data from ReplicateSummary
#'@param graphic.title general title for all conditions. individual condition name will be appended.
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_conds <- function(data.combined.summarized, graphic.title, path){
  er
  if(FALSE == (dir.exists(paste(wd, "Figures", sep="/")))){
    dir.create((paste(wd, "Figures", sep="/")))
  }
  if(FALSE == (dir.exists(paste(wd, "Figures","SVGs", sep="/")))){
    dir.create((paste(wd, "Figures", "SVGs", sep="/")))
  }

  print("generating graphics, please be patient")


  for(cond in unique(data.combined.summarized$Condition)){
    sub1 <- subset(data.combined.summarized, Condition == cond)

    p <- ggplot2:ggplot(data.combined.summarized, aes(x=Time, y=OD600, group=Strain, colour=Strain))+
      geom_errorbar(aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
      geom_line(size=3)+
      theme(legend.title =element_text(size = 25, face="bold" ),
            legend.text= element_text(size=20, face="bold"),
            title=element_text(size= 25, face= "bold"),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            axis.ticks.length = unit(0.3, "cm"))+
      labs(title = paste(graphic.title, cond, sep="+"),
           x="Time(h)",
           y="Cell Density (OD600)", element_text(size=15, face="bold")+
      scale_x_continuous(breaks = breaks_extended(n=10)))


    ggplot2::ggsave(paste((if(FALSE %in% grepl("%", cond)){cond}else{sub( "%", " percent",cond)}), "jpeg", sep="."), path=paste(path, "Figures", sep=""), width = 13, height= 8, device="jpeg", plot = p )

    ggplot2::ggsave(paste(cond, "svg", sep="."), path=paste(path, "Figures/", "SVGs", sep=""), width = 13, height= 8, plot = p)

    print("if error says 'Removed n rows containing missing values (geom_errorbar)', and you have only 1 replicate per condition, please ignore this error")

  }
}


#'Plot summary of all conditions
#'
#'Uses ggplot2's facet_wrap function to plot all conditions and saves as jpeg and svg. This
#'
#'@param data.combined.summarized tidy, long data from SummarizeDataCombined
#'@param graphic.title what you would like to title this graphic
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_wrap <- function(data.combined.summarized, graphic.title, path){

  if(FALSE == (dir.exists(paste(path, "Figures", sep="/")))){
    dir.create((paste(wd, "Figures", sep="/")))
  }
  if(FALSE == (dir.exists(paste(path, "Figures","SVGs", sep="/")))){
    dir.create((paste(wd, "Figures", "SVGs", sep="/")))
  }

  p <- ggplot2::ggplot(data.combined.summarized, aes(x=Time, y=OD600, group=Strain, colour=Strain))+
    facet_wrap(~Condition)+
    geom_errorbar(aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
    geom_line(size=2)+
    theme(legend.title =element_text(size = 15, face="bold" ),
          legend.text= element_text(size=15, face="bold"),
          title=element_text(size= 20, face= "bold"),
          strip.text.x =  element_text(size=12),
          axis.ticks.length = unit(0.3, "cm"))+
    labs(title= graphic.title,
         x="Time(h)",
         y="Cell Density (OD600)",
         element_text(size=15, face="bold")+
    scale_x_continuous(breaks = breaks_extended(n=10)))

  ggplot2::ggsave("Facet_Wrap.jpeg", path = paste(path, "Figures", sep=""), width = 13, height= 8, device="jpeg", plot = p)
  ggplot2::ggsave(paste("Facet_Wrap.svg"), path = paste(path, "Figures/", "SVGs", sep=""),width = 13, height= 8, plot = p)

  print("check wd for new 'Figures' folder, containing generated graphics")
  print("if error says 'Removed n rows containing missing values (geom_errorbar)', and you have only 1 replicate per condition, please ignore this error")
}


#'Wrap individual biological replicates to spot-check
#'
#'Subsets each biological replicate and plots a facet_wrap with every condition. this allows you to spot check the biological reps to see if there is any obvious problems.
#'
#'@param data.combined.summarized tidy, long data from SummarizeDataCombined
#'@param title what you would like to title this graphic
#'@param path path to the folder where you would like to store these pictures -- should end in "/"
#'@export
GCGplot_bioreps <- function(data.combined.summarized, title, path){

  bio.reps.list <- unique(data.combined.summarized$Bio_Rep)

      data.combined.summarized.no.empty.brep <- subset(data.combined.summarized, Bio_Rep == bio.reps.list[i])
      colnames(data.combined.summarized.no.empty.brep)[3] <- "Time"

      data.combined.summarized.tidy.brep <- deplyr::ddply(data.combined.summarized.no.empty.brep, c("Strain", "Condition", "Time"), summarise,
                                       N    = sum(!is.na(OD600)),
                                       mean = mean(OD600,na.rm=TRUE),
                                       sd   = sd(OD600,na.rm=TRUE),
                                       se   = sd / sqrt(N))

      tech.rep.only <- "Showing only technical replicates"

      data.combined.summarized.tidy.brep<- data.combined.summarized.tidy.brep%>% deplyr::rename("OD600" = "mean")
      data.combined.summarized.tidy.brep$Time <- as.numeric(as.character(data.combined.summarized.tidy.brep$Time))
      data.combined.summarized.tidy.brep$Time <- data.combined.summarized.tidy.brep$Time / 60 #make it per hr.
      data.combined.summarized.tidy.brep$Time <- factor(data.combined.summarized.tidy.brep$Time)
      data.combined.summarized.tidy.brep$Condition <- factor(data.combined.summarized.tidy.brep$Condition)
      data.combined.summarized.tidy.brep$OD600 <- as.numeric(data.combined.summarized.tidy.brep$OD600)
      data.combined.summarized.tidy.brep$Time <- as.numeric(as.character(data.combined.summarized.tidy.brep$Time))

     p <-  ggplot2::ggplot(data.combined.summarized.tidy.brep, aes(x=Time, y=OD600, group=Strain, colour=Strain))+
        facet_wrap(~Condition)+
        geom_errorbar(aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
        geom_line(size=2)+
        theme(legend.title =element_text(size = 15, face="bold" ),
              legend.text= element_text(size=15, face="bold"),
              title=element_text(size= 20, face= "bold"),
              strip.text.x =  element_text(size=12),
              axis.ticks.length = unit(0.3, "cm"))+
        scale_colour_manual(values=cbp2)+
        labs(title= paste(Graphic.title,"rep", bio.reps.list[i]),
             x="Time(h)",
             y="Cell Density (OD600)",
             element_text(size=15, face="bold")+
        scale_x_continuous(breaks = breaks_extended(n=10)))

     print(p)

      ggplot2::ggsave(paste("Facet_Wrap_biorep" ,i, ".jpeg"), path=paste(wd, "Figures", sep="/"), width = 13, height= 8, device="jpeg", plot = p  )
      ggplot2::ggsave(paste("Facet_Wrap_biorep", i, ".svg"), path=paste(wd, "Figures", "SVGs", sep="/"),width = 13, height= 8, plot= p)

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
  plate.names <- unique(data.combined.summarized.no.empty$plate.name)

  for(i in 1:length(plate.names)){

    print(i)

    single.plate.data <- subset(data.combined.summarized.no.empty, plate.name == plate.names[i])

    single.plate.data$Time <- as.numeric(single.plate.data$Time)
    single.plate.data$Coordinate <- factor(single.plate.data$Coordinate, levels = unique(single.plate.data$Coordinate))



    p <- ggplot2::ggplot(single.plate.data , aes(x=Time, y=OD600, group=Strain, colour=Strain,))+
      facet_wrap(~Coordinate,
                 ncol = 10 )+
      geom_line(size=2)+
      theme(legend.title =element_text(size = 15, face="bold" ),
            legend.text= element_text(size=15, face="bold"),
            title=element_text(size= 20, face= "bold"),
            strip.text.x =  element_text(size=12),
            axis.ticks.length = unit(0.3, "cm"))+
      scale_colour_manual(values=cbp2)+
      labs(title= paste(Graphic.title, "plate", plate.names[i]),
           x="Time(h)",
           y="Cell Density (OD600)",
           element_text(size=15, face="bold"),
           caption=tech.rep.only)+
      scale_x_continuous(breaks = breaks_extended(n=10))

    print(p)

    ggplot2::ggsave(paste("plate matrix", plate.names[i], ".jpeg"), path=paste(wd, "Figures", sep="/"),width = 13, height= 8, plot = p)

  }
}
