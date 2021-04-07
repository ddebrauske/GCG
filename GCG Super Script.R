#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#  Made By Derek J. Debrauske, 2021
#  ddebrauske@gmail.com
#  ddebrauske@wisc.edu

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



#Update, install and attach GCG package.
detach(package:GCG)
remove.packages("GCG")#remove GCG package from R root folder so you can update it.
#remove GCG package from workspace so you can update it.
devtools::install_github("ddebrauske/GCG", force=TRUE)#install GCG package from github
library(GCG) #add package to current R environment


#run "help()" commands to see more information on each function
#run "head()" commands to preview data


#Optional - Set the working directory to the folder you'd like to work from
setwd("C:/Users/Derek Debrauske/Dropbox/R/Projects/20201023 GCG Arl1 in phenolics redo rep/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Act I: Gathering data
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
# Returns a similar, long dataframe with wells as rows.
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
#Optional step -- if your plate reader data is exported from your plate reader as a workbook with many sheets, you can use this function to separate these sheets into separate .csv files to be used in the next steps. (uncomment to use)
# help("ExcelToCSV")
# ExcelToCSV(path = "", out_dir = "")
# list.files("")

#~~~~~~~~~~~~
#4. Import plate reader data from .csv files into R.
#make if( no blank ){ don't subtract}
help("Import")
Timepoint.data <- Import(path = "Plate_reader_data/", plate.reader.type = "spark", read.interval = 60)
head(Timepoint.data, n=12)

#~~~~~~~~~~~~
#5. attach information from layout and blank to plate reader data. this function back-subtracts the blank values from the OD600 of corresponding wells.
help("TimeseriesLayoutBlank")
# You can run this one of two ways:

#     1. Without back subtracting blanks: use only
data.combined.no.blank <- TimeseriesLayoutBlank(timepoint.df = Timepoint.data, layout.df = layout)
head(data.combined.no.blank)

#     2. With back-subtracting blanks:
data.combined <- TimeseriesLayoutBlank(timepoint.df= Timepoint.data, layout.blank.df = layout.blanks)


#5.1 subset out all strains labeled as "ddH2O" these are my border wells
data.combined <- subset(data.combined, Strain != "ddH2O")
head(data.combined) #you can see now that there are no more "ddH2O" wells, and the row A (border) has been skipped.

#~~~~~~~~~~~~
#6. Summarize technical replicates
#this finds mean, SE of biological replicates. if there are technical replicates, they are averaged first, then mean and SE is calculated from these means.
help("SummarizeTechreps")
Tech.Rep.Summary <- SummarizeTechReps(data.combined)
head(Tech.Rep.Summary)

#~~~~~~~~~~~~
#7. Summarize Biological Replicates
help("SummarizeBioReps")
Bio.Rep.Summary <- SummarizeBioReps(Tech.Rep.Summary)
head(Bio.Rep.Summary)





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


#  ______  __ __    ___        ___  ____   ___
# |      ||  |  |  /  _]      /  _]|    \ |   \
# |      ||  |  | /  [_      /  [_ |  _  ||    \
# |_|  |_||  _  ||    _]    |    _]|  |  ||  D  |
#   |  |  |  |  ||   [_     |   [_ |  |  ||     |
#   |  |  |  |  ||     |    |     ||  |  ||     |
#   |__|  |__|__||_____|    |_____||__|__||_____|
