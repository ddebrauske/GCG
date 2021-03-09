#'A function to parse through a plate matrix style table and return a "long" dataframe, with each "well" as a row. this function is used inside of the PlateLayout() and PlateBlank() function.
#'@param inDF dataframe to be parsed from plate matrix format into tidy "long" format
#'@return out_df, a tidy, long style dataframe with your plate information
#'@export
ParsePlateLayout <- function(inDF){
  column_names = inDF[1,2:ncol(inDF)]#extract column names from first row skipping field 1
  column_names <- (t(column_names))
  column_names <- factor(column_names)
  row_names = inDF[2:nrow(inDF),1] #extract row names from first column skipping field 1
  Content = inDF[2:nrow(inDF),2:ncol(inDF)] #extract content from matrix

  #generate vectors of row and column values
  Row = as.character(rep(row_names,each=length(column_names)))
  Column = as.character(rep(column_names,each=length(row_names)))
  levels(column_names)

  Xval <- as.character(Row)
  Yval <- as.character(column_names)
  Content <- as.character(t(Content))
  out_df <- data.frame(Xval, Yval,Content)
  out_df <- as.data.frame((out_df), rownames=TRUE)
  return(out_df)
}
