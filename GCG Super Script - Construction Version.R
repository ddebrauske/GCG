#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#  Made By Derek J. Debrauske, 2021
#  ddebrauske@gmail.com
#  ddebrauske@wisc.edu

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Update, install and attach GCG package.

remove.packages("GCG")#remove GCG package from R root folder so you can update it.
detach(package:GCG)#remove GCG package from workspace so you can update it.
devtools::install_github("ddebrauske/GCG", force=TRUE)#install GCG package from github
library(GCG) #add package to current R environment


#run "help()" commands to see more information on each function
#run "head()" commands to preview data


#Optional - Set the working directory to the folder you'd like to work from
setwd("C:/Users/Derek Debrauske/Dropbox/R/Projects/20210303 Chemgen validation R2/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Act I: Gathering data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~
#1. Step through plate layout input file, edited from template
#     This function returns a long data frame, combining all of the information from your plate layouts. Each row of this data frame refers to one well.
layout <- PlateLayout("Plate_Layout.csv") #change this path to refer to th elocation of your Plate_layout file.

#~~~~~~~~~~~~
#2. Do the same with the blank input file.
#     Returns a similar, long dataframe with wells as rows.
blank <- PlateBlank("Plate_Blanks.csv")  #change this path to refer to th elocation of your Plate_blank file.

#~~~~~~~~~~~~
#3. Combine all of the above information into one table.
layout.blanks <- CombineLayoutBlank(layoutDF = layout, blankDF = blank)

#~~~~~~~~~~~~
#Optional step -- if your plate reader data is exported from your plate reader as a workbook with many sheets, you can use this function to separate these sheets into separate .csv files to be used in the next steps. (uncomment to use)

# ExcelToCSV(path = "", out_dir = "")
# list.files("")

#~~~~~~~~~~~~
#4. Import plate reader data from .csv files into R.
Timepoint.data <- Import(path = "Plate_reader_data/", plate.reader.type = "spark", read.interval = 60)

#~~~~~~~~~~~~
#5. attach information from layout and blank to plate reader data. this function back-subtracts the blank values from the OD600 of corresponding wells.
# You can run this one of two ways:
#     1. Without back subtracting blanks: use only 
data.combined.no.blank <- TimeseriesLayoutBlank(timepoint.df = Timepoint.data, layout.df = layout)

#     2. With Back-subtracting blanks:
data.combined <- TimeseriesLayoutBlank(timepoint.df= Timepoint.data, layout.blank.df = layout.blanks)

#5.1 subset out all strains labeled as "ddH2O" these are my border wells
data.combined <- subset(data.combined, Strain != "ddH2O")


#~~~~~~~~~~~~
#6. Summarize technical replicates
#this finds mean, SE of biological replicates. if there are technical replicates, they are averaged first, then mean and SE is calculated from these means.
Tech.Rep.Summary <- SummarizeTechReps(data.combined)

#~~~~~~~~~~~~
#7. Summarize Biological Replicates

Bio.Rep.Summary <- SummarizeBioReps(Tech.Rep.Summary)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Act II: Plotting Curves
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~
# Plot all replicate data as a wrap of all conditions
help("GCGplot_wrap")
p <- GCGplot_wrap(Bio.Rep.Summary, path= "./", graphic.title = "ChemGen Validation R2")
print(p)

#~~~~~~~~~~~~
# Plot individual conditions
help("GCGplot_conds")
GCGplot_conds(Bio.Rep.Summary, graphic.title ="ChemGen Validation R2", path= "./")#see results in folder


#~~~~~~~~~~~~
#plot all wells to spot-check plates.
help("GCGplot_matrices")
GCGplot_matrices(data.combined, path= "./" , graphic.title = "ChemGen Validation R2" )#see results in folder


#~~~~~~~~~~~~
#plot each biological rep as a separate facet_wrap
help("GCGplot_bioreps")
GCGplot_bioreps(data.combined, path = "./", graphic.title = "ChemGen Validation R2")#see results in folder





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Act III: Analyzing Results Using GrowthCurver and other functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #~~~~~~~~~~~~
# # Convert data.combined plate reader data into growthcurver format.this is only if you want to go straight from data.combined.
# help("Growthcurver_convert")
# d <- Growthcurver_convert(data.combined) #output is /.growthcurverfile.csv
#
# #~~~~~~~~~~~~
# #Use Growthcurver to analyze raw data
# gc_plate <- growthcurver::SummarizeGrowthByPlate(d)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Act IV: Analyzing Biological replicates individually with Growthcurver and findgr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~
#Analyze biological replicates seperately with Growthcurver

data.combined.gcr <- matrix(data= NA)
data.combined.gcr <- Tech.Rep.Summary

data.combined.gcr[, ncol(data.combined.gcr) + 1 ] <- mapply(paste, sep= "@", data.combined.gcr$Strain , data.combined.gcr$Condition, data.combined.gcr$Bio_Rep) #combines all strain, cond, bio rep infomation into one identifying column
data.combined.gcr <- data.combined.gcr[, c(ncol(data.combined.gcr),3:(ncol(data.combined.gcr)-1))] #reorders columns to be easier to look at , ID being in column 1
colnames(data.combined.gcr)[1] <- "Strain.Cond.Rep"
colnames(data.combined.gcr)[2] <- "time" #needs to be lowercase for growthcurver format
data.combined.gcr <- data.combined.gcr[c(1,2,5)]
data.combined.gcr.wide <- tidyr::pivot_wider(data.combined.gcr, values_from = OD600, names_from = "Strain.Cond.Rep")


#~~~~~~~~~~~~~~~~
#Get curve info from growthcurver
gc.bio.reps <- growthcurver::SummarizeGrowthByPlate(data.combined.gcr.wide)

gc.bio.reps <- tidyr::separate(data=gc.bio.reps,col = sample, into = c("Strain", "Condition", "Bio_Rep"), sep= "@" , )















#@@@ Under Construction
#~~~~~~~~~~~~~~~~
#Find maximum growth rate of curve using ln() transformation and sliding window


#This script contains a function that can be used to find the growth rate of
#a set of points grown on the Biotek plate reader.

#x = a vector of data points, e.g. c(0.1, 0.1, 0.2, 0.25, 0.3, ...)
#t = a vector of time points, e.g. c(0, 15, 30, 45, 60, ...)
#plottitle = the title to go on the plot of the fit
#int = the number of time steps that should be used to fit the line
#r2.cutoff = how stringent the fit needs to be --> 1 is the max.


d <- data.combined.gcr.wide

summary1 <- as.data.frame(matrix(NA, nrow=ncol(d)-1, ncol=5))
summary1[1] <- colnames(d)[2:ncol(d)]
colnames(summary1) <- c("Condition", "m", "r2", "lag", "note")

#source("C:/Users/ddebr/Dropbox/R/Functions/find_gr.R")
path= "C:/Users/Derek Debrauske/Dropbox/R/Projects/20210330 GCG superscript testing/20210303 Chemgen validation R2/Figures/GR/" #path where you would like to save figures


for(j in 2:ncol(d)){

  jpeg(filename = paste(path, if(FALSE %in% grepl("%", colnames(d)[j])){
    colnames(d)[j] #if there is a "%" in what will be the file name, replace with "percent"
  }else{
    sub( "%", " percent", colnames(d)[j])}, ".jpeg"))

  x = as.vector(d[[j]])
  t=d$time
  plottitle = colnames(d)[j]
  r2.cutoff = 0.995
  int = 5



  #findgr = function(x, t, plottitle, int=8, r2.cutoff=0.997) {
    x = as.numeric(x)
    n = length(x)
    mat = NULL
    max = c(0,0,0,NA)

    #are x and t the same length?
    if (length(x) != length(t)) {
      cat("Error: Your data and time are not the same length.\n")
      stop()
  
    }

    #is the line basically flat?
    fit = lm(x~t)
    m = abs(coefficients(fit)[[2]])
    if (m < 0.00001) {
      max=c(0,0,0,NA)
      lag=NA
      summary1$note[j-1] <- paste(summary1$note[j-1], "no growth")

      plot(t, log(x), pch=20, xlab="time", ylab="ln(OD600)", main=plottitle)
      mtext("no growth", side=3, line=-1, at=0, cex=0.8, adj=0)
      print(paste(j, "-- no growth"))
    }else{#if not, find a slope
      x[which(x <= 0)] = 0.001 	#transform values < 0

      x = log(x)
      for (i in 1:(n-int)) {
        fit = lm(x[i:(i+int)]~t[i:(i+int)])		#linear regression on log transformed data.
        m = coefficients(fit)[[2]]
        b = coefficients(fit)[[1]]
        r2 = summary(fit)$r.squared
        mat = rbind(mat, c(i, b, m, r2))
      }
      mat = mat[which(mat[,4] > r2.cutoff),] #only include slopes greater than the R2 cutoff.
      
      if(is.matrix(mat) && dim(mat)[1] != 0 && dim(mat)){ #DD modified 20210331 -- added this point to ignore data that is not within the R2 cutoff. before adding this, code would break due to having a matrix subset with no dimensions. 
      
       
      max = mat[which.max(mat[,3]),]
      par(las=1, mar=c(5, 4, 4, 4) + 0.1)
      plot(t,x, type="n", pch=20, xlab="time", ylab="ln(OD600)", main=plottitle)
      usr.old = par("usr")

      #how long is this in exponential growth?
      fit.line = sapply(t, function(x) max[3]*x+max[2])
      resid = fit.line-x
      residper = abs(resid/fit.line)
      resid.mat = rbind(t, fit.line, x, resid, residper)
      resid.mat = resid.mat[,which(resid.mat[5,] < 0.05)]
      lag = resid.mat[1,1]


      mtext(paste("lag =",round(lag,2)),side=3, line=-3, at=0, cex=0.8, adj=0)
      time.in.exp = resid.mat[1,ncol(resid.mat)]-resid.mat[1,1]
      abline(v=lag, col="cadet blue", lty=2)
      abline(v=resid.mat[1,ncol(resid.mat)], col="cadet blue", lty=2)


      #plotting instantaneous growth rate   -- DD uncommented
      dx = diff(x)/(t[2]-t[1])
      par(usr=c(par("usr")[1:2],min(dx)*1.05, max(dx)*1.05))
      points(t[1:(length(t)-1)],dx, pch=18, type="o", col="dark grey", lty=1)
      axis(4, col.axis="dark grey", col.ticks="dark grey")
      mtext("delta(x)/delta(t)", side=4, line=3, col="dark grey", las=3)

      #plot
      par(usr=usr.old)
      points(t,x,pch=20)
      abline(lm(x[max[1]:(max[1]+int-1)]~t[max[1]:(max[1]+int-1)]), col="red", lty=2, lwd=2)
      points(t[max[1]:(max[1]+int-1)], x[max[1]:(max[1]+int-1)], col="red")
      mtext(paste("m =",round(max[3],3)), side=3, line=-1, at=0, cex=0.8, adj=0)
      mtext(paste("r2 =",round(max[4],4)), side=3, line=-2, at=0, cex=0.8, adj=0)
      #mtext(colnames(d)[j])
      
      }else{
        summary1$note[j-1] <- paste(summary1$note[j-1],"- below r2 cutoff")
      }
}
gr.data <- c("m"=max[3], "r2"=max[4], "lag"=lag)


#gr.data<- findgr(x = as.vector(d[[i]]), t=d$time, plottitle = NA, r2.cutoff = 0.994, int = 3)
summary1$m[j-1] <- gr.data[[1]]
summary1$r2[j-1] <- gr.data[[2]]
summary1$lag[j-1] <- gr.data[[3]]


dev.off() #saves JPEG device. 
}




# #@@@@@@@@@@@@@@@@@@@@@@@
# source("C:/Users/ddebr/Dropbox/R/Functions/find_gr.R")
#
# summary1 <- as.data.frame(matrix(NA, nrow=ncol(d)-1, ncol=4))
# summary1[1] <- colnames(d)[2:ncol(d)]
# colnames(summary1) <- c("Condition", "m", "r2", "lag")
#
# for(j in 2:ncol(d)){
#
#   gr.data<- findgr(x = as.vector(d[[j]]), t=d$time, plottitle = NA, r2.cutoff = 0.990, int = 3)
#   summary1$m[j-1] <- gr.data[[1]]
#   summary1$r2[j-1] <- gr.data[[2]]
#   summary1$lag[j-1] <- gr.data[[3]]
#
# }
#





#~~~~~~~~~~~~~~~~~
#add information from layout.blanks to plate.summary
# plate_summary$Strain <- NA
# plate_summary$Condition <- NA
#
#
# #write for() loop pulling condition and strain information from layout.blanks
# #copied and edited from combine layout.blank
# for(i in 1:nrow(plate_summary)){ #step through layout blanks row by row
#   for(j in 1:nrow(layout.blanks)){ #step through blank.out until you find a matching coordinate&&plate#
#     summ.coord <- plate_summary$coordinate[i]
#     layout.coord <- layout.blanks$Coordinate[j]
#
#     summ.plate.number <- as.character(plate_summary$plate[i])
#     layout.plate.number <- as.character(layout.blanks$Plate[j])
#
#
#     if(layout.coord == summ.coord && layout.plate.number == summ.plate.number){ #if they match, add OD600 value to the new column of row i
#       plate_summary$Strain[i] <- layout.blanks$Strain[j]
#       plate_summary$Condition[i] <- layout.blanks$Condition[j]
#     }
#   }
# }
#

#for(i in 1:length(dev.list())){dev.off()}
setwd("C:/Users/Derek Debrauske/Documents/R/GCG/")
devtools::document()

