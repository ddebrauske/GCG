remove.packages("GCG")#remove GCG package from R root folder so you can update it.
detach(package:GCG)#remove GCG package from workspace so you can update it.
devtools::install_github("ddebrauske/GCG", force=TRUE)#install GCG package from github
library(GCG) #add package to current R environment


#run "help()" commands to see more information on each function




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##gathering data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~
#1. Step through plate layout input file, edited from template
#     This function returns a long data frame, combining all of the information from your plate layouts. Each row of this data frame refers to one well. 

help("PlateLayout") 
layout <- PlateLayout("Plate_Layout.csv") #change this path to refer to th elocation of your Plate_layout file. 
head(layout, n = 12) #run this to see what the converted format looks like. 
unique(layout$Plate)

#~~~~~~~~~~~~
#2. Do the same with the blank input file. 
#     Returns a similar, long dataframe with wells as rows.
help("PlateBlank")#Converts matrix style blanks from template into long table
blank <- PlateBlank("Plate_Blanks.csv")  #change this path to refer to th elocation of your Plate_blank file. 
head(blank, n=12) #check out what the output format looks like
unique(blank$Plate) #check that you got all your plates
 
#~~~~~~~~~~~~
#3. Combine all of the above information into one table.
help("CombineLayoutBlank")
layout.blanks <- CombineLayoutBlank(layoutDF = layout, blankDF = blank)
head(layout.blanks, n=12)

#~~~~~~~~~~~~
#Optional step -- if your plate reader data is exported from your plate reader as a workbook with many sheets, you can use this function to separate these sheets into separate .csv files to be used in the next steps.
help("ExcelToCSV")
ExcelToCSV(path = "DK_96well_24hr_stckr_20210308_165850_davestoppedm-- R2.C -- labeled and trimmed to 48hr.xlsx", out_dir = "Plate_reader_data/")
list.files("Plate_reader_data/")

#~~~~~~~~~~~~
#4. Import plate reader data from .csv files into R. 
#make if( no blank ){ don't subtract}
help("Import")
Timepoint.data <- Import(path = "Plate_reader_data/", plate.reader.type = "spark", read.interval = 60)
head(Timepoint.data, n=12)

#~~~~~~~~~~~~
#5. attach information from layout and blank to plate reader data. this function back-subtracts the blank values from the OD600 of corresponding wells. 
#make if( no blank ){ don't subtract}
help("TimeseriesLayoutBlank")
data.combined <- TimeseriesLayoutBlank(timepoint.df = Timepoint.data, layout.blank.df = layout.blanks)
head(data.combined, n=12)

#i will subset out all strains labeled as "ddH2O" these are my border wells
data.combined <- subset(data.combined, Strain != "ddH2O")
head(data.combined) #you can see now that there are no more "ddH2O" wells, and the row A (border) has been skipped.

#~~~~~~~~~~~~
#6. Summarize replicates
#this finds mean, SE of biological replicates
help("ReplicateSummary")
data.combined.summary <- ReplicateSummary(data.combined)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Plotting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~
# Plot data using ggplot
help("GCGplot_wrap") #not yet working with .svg files
p <- GCGplot_wrap(data.combined.summary, path= "C:/Users/ddebr/Desktop/", graphic.title = "ChemGen Validation")
print(p)

#~~~~~~~~~~~~
# Plot individual conditions
help("GCGplot_cond")
GCGplot_cond(data.combined.summary, graphic.title ="ChemGen Validation", path= "C:/Users/ddebr/Desktop/")#see results in folder

#~~~~~~~~~~~~
#plot all wells to spot-check plates.
help("GCGplot_matrices")
GCGplot_matrices(data.combined, path= "C:/Users/ddebr/Desktop/" , graphic.title = "ChemGen Validation R2" )#see results in folder


#~~~~~~~~~~~~
#plot each biological rep as a separate facet_wrap
help("GCGplot_bioreps")
GCGplot_bioreps(data.combined, path = "C:/Users/ddebr/Desktop/", graphic.title = "ChemGen Validation R2")#see results in folder





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##growthcurver
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~
#Optional -- Convert plate reader data into growthcurver format.this is only if you want to go straight from data.combined.
##make if( no blank ){ don't subtract}
#converting into growthcurver format
help("Growthcurver_convert")
Growthcurver_convert(data.combined.summary) #output is /.growthcurverfile.csv



#need to have header name variable to convert row names into column names.
#summarize only technical replicates do pipe into growrthcurver


#summarizes technical replicates before piping into growthcurver.
tech.reps.averaged <- plyr::ddply(data.combined, c("Strain", "plate.name", "Condition", "Time", "Bio_Rep"), plyr::summarise,
                                  N    = sum(!is.na(OD600)),
                                  mean = mean(OD600,na.rm=TRUE))


#convert to gcg format, combine columns strain%condition%biorep
data.combined.gcr <- matrix(data= NA)
data.combined.gcr <- tech.reps.averaged

data.combined.gcr[, ncol(data.combined.gcr) + 1 ] <- mapply(paste, sep= "@", data.combined.gcr$Strain , data.combined.gcr$Condition, data.combined.gcr$Bio_Rep)
data.combined.gcr <- data.combined.gcr[, c(ncol(data.combined.gcr),3:(ncol(data.combined.gcr)-1))]
colnames(data.combined.gcr)[1] <- "Strain.Cond.Rep"
colnames(data.combined.gcr)[3] <- "time" #needs to be lowercase for growthcurver format
data.combined.gcr <- data.combined.gcr[c(1,3,6)]
data.combined.gcr.wide <- tidyr::pivot_wider(data.combined.gcr, values_from = mean, names_from = "Strain.Cond.Rep")
write.table(data.combined.gcr.wide, file= "./growthcurverfile.csv",sep = "\t", row.names = FALSE)

#Growthcurver summary -- file created with Growthcurver_convert

file_name <- "./growthcurverfile.csv"
d <- read.table(file_name, header=TRUE, sep= "\t", stringsAsFactors = FALSE, check.names = FALSE)

gc_plate <- growthcurver::SummarizeGrowthByPlate(d)

biorep.summary <- tidyr::separate(col = sample, into = c("Strain", "Condition", "BioReplicate"), sep= "@" , data=gc_plate)






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





