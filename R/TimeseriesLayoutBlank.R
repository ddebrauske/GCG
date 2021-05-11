#'Combine information from plate layout and blank reading
#'
#'Adds together your dataframes from CombineLayoutBlank and Import to create a very long, tidy dataframe that can be used for plotting and other analysis.
#'
#'This can be used 2 ways: with or without blank back-subtraction.
#'
#'
#'@param timepoint.df the dataframe you received from Import() that contains your long, tidy time series data
#'@param layout.blank.df use this option if you wish to back-subtract blanks. Use output from CombineLayoutBlank() that contains long, tidy plate layout information
#'@param layout.df Use this if you do not wish to back-subtract blanks. Use output from PlateLayout()
#'@return data.combined. a very long, tidy df with all of your data, corrected by back-subtracting the blank values.
#'@export
TimeseriesLayoutBlank <- function(timepoint.df,layout.blank.df=NULL, layout.df=NULL){
  if(is.null(layout.blank.df) && !is.null(layout.df)){
    layout.blank.df <- layout.df
    layout.blank.df$Blank_OD600 <- 0.00
    layout.blank.df$Plate <- factor(layout.blank.df$Plate)#if you don't use the blank, we'll just change the name, column names should be the same
    print("Warning: no blank!")
  }

   #checkpoint to ensure layout blank and time seres data are aligning.
    print("checking to see if layout, blank, and time series data match names of plates/datafiles.")
    if(FALSE == (length(unique(timepoint.df$plate.name)) == length(unique(layout.blank.df$Plate)))){
      stop(":( Mismatch between ammount of plates in layout/blanks and Plate reader data files!

           ")
    }else{
      print( ":) ammount of plates in layout/blanks match plate reader data")


      # then check to see unique names match
      if(FALSE %in% (unique(timepoint.df$plate.name) == levels(layout.blank.df$Plate))){
        stop("Plate names do not match!")
      }else{
        print(":) Blank and layout match plate names")
      }
    }
    print("Checkpoint 2 passed")


  data.combined <- as.data.frame(matrix(ncol=ncol(timepoint.df)+3,nrow= nrow(timepoint.df), data=NA)) #create a new dataframe that will eventually have subtracted OD value, strain and condision information.
  data.combined[,1:ncol(timepoint.df)] <- timepoint.df
  colnames(data.combined)[1:ncol(timepoint.df)] <- colnames(timepoint.df)
  colnames(data.combined)[ncol(timepoint.df)+1] <- "Strain"
  colnames(data.combined)[ncol(timepoint.df)+2] <- "Condition"
  colnames(data.combined)[ncol(timepoint.df)+3] <- "Bio_Rep"



  print("Combining layouts, blanks and plate reader dataframes. This will take a minute to run, please be patient")

  for(i in 1:nrow(data.combined)){
    ##add strain and condition and Replicate info to data.combined table. Also subtract blank values based on coordinate and plate #
    for(j in 1:nrow(layout.blank.df)){
      if(data.combined$Coordinate[i] == layout.blank.df$Coordinate[j] && data.combined$plate.name[i]==as.character(layout.blank.df$Plate[j])){
        data.combined$Strain[i] <- as.character(layout.blank.df$Strain[j])
        data.combined$Condition[i] <- as.character(layout.blank.df$Condition[j])
        data.combined$OD600[i] <- as.numeric(as.character(data.combined$OD600[i])) - as.numeric(as.character(layout.blank.df$Blank_OD600[j]))
        data.combined$Bio_Rep[i] <- as.character(layout.blank.df$Bio.Rep[j])
      }
    }
  }

  data.combined$OD600 <- as.numeric(data.combined$OD600)
  data.combined <- dplyr::rename( "Time" = "Timepoint", .data=data.combined)
  data.combined$Time <- as.numeric(data.combined$Time)

  return(data.combined)
}
