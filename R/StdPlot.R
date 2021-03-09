#'simple ggplot settings used to plot growth curves with 2 variables (Strain/Condition) over time.
#'
#'@param inDF tidy, long dataframe from TimeseriesLayoutBlank() function or same format
#'@param title umbrella tile of your plots
#'@param Read.interval.in.min read interval, in minutes of your plate experiment. one read every hour is Read.interval.in.min = 60
#'@export p prints graphic
std_growth_curve_plot <- function(inDF, title, Read.interval.in.min){
  p= NA
  p <- ggplot(inDF, aes(x=Time, y=OD600, group=Strain, colour=Strain))+
    geom_errorbar(aes(ymin=OD600-se, ymax=OD600+se), width=.1)+
    geom_line(size=3)+
    scale_colour_manual(values=cbp2)+
    theme(legend.title =element_text(size = 25, face="bold" ),
          legend.text= element_text(size=20, face="bold"),
          title=element_text(size= 25, face= "bold"),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.ticks.length = unit(0.3, "cm"))+
    labs(title= title,
         x="Time(h)",
         y="Cell Density (OD600)", element_text(size=15, face="bold"),
         caption = tech.rep.only)+
    scale_x_continuous(breaks = breaks_extended(n=10))
  print(p)
  #use 1744 X 1066 pixels or 13X8 inches
  return(p)
}
