#'Combine plate layout and blank
#'
#'This function combines the information in platelayout data frame created with PlateLayout and the blank dataframe created with PlateBlank by looping through and matching Coordinate/Plate IDs.
#'
#'@param layoutDF is your "long" layout dataframe parsed out from your plate layout matrix
#'@param blankDF is the same thing but your blanks
#'@return a dataframe of paired layout and blank information
#'@export
CombineLayoutBlank <- function(layoutDF, blankDF){


      #First we'll do a checkpoint to make sure things are lining up


      layoutDF$Plate <- factor(layoutDF$Plate)
      blankDF$Plate <- factor(blankDF$Plate)

    print("checking to see if layout and blank DFs match")
      if(nrow(layoutDF) == nrow(blankDF)){ #check to see if layout and blank match lengths
        print(":-) Blank and layout match lengths")
        if(FALSE %in% (levels(layoutDF$Plate) == levels(blankDF$Plate))){ #check to see if layout and blank   match levels
          stop(" :-(  Plate names do not match!")
        }else{
          print( ":-) plate names match")
        }
      }else{
        stop(" :-( number of plates do not match!")
      }
      print(":-) Checkpoint passed")

    layout.blanks <- matrix(ncol=1, nrow=nrow(layoutDF)) ## this new dataframe will be the combination of condition, strain, coordinate, Biological rep and plate#. it will combine the two dataframes generated above by matching up coordinate and plate# columns row by row and add the respective OD600 value to df.

    layout.blanks <- as.data.frame(layout.blanks)
    layout.blanks <- cbind(layout.blanks, layoutDF)
    names(layout.blanks)[1] <- "Blank_OD600"

    layout.blanks <- layout.blanks[,c(-3,-4)] #Removes X and Y matrix value from well nformation, because this now exicsts as a combined "coordimate" A1, for example.

    print("this may take some time, please be patient")

    for(i in 1:nrow(layout.blanks)){ #step through layout blanks row by row
      for(j in 1:nrow(blankDF)){ #step through blank.out until you find a matching coordinate&&plate#
        layout.coord <- layoutDF$Coordinate[i]
        blank.coord <- blankDF$Coordinate[j]
        layout.plate.number <- as.character(layout.blanks$Plate[i])
        blank.plate.number <- as.character(blankDF$Plate[j])


        if(layout.coord == blank.coord && layout.plate.number == blank.plate.number){ #if they match, add OD600 value to the new column of row i
          layout.blanks$Blank_OD600[i] <- as.numeric(as.character(blankDF$OD600[j]))
        }
      }
    }

  layout.blanks$Plate <- as.character(layout.blanks$Plate) #needed to return to character vector for Annotation script
  return(layout.blanks)

  }
