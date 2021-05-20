#'eAUC -- Emperical area under the curve
#'
#'Finds the area of the curve of a set of points by integrating every point along your curve as an area value. This function is the core function of the Growthcurver package's EmpericalAreaUnderTheCurve function
#'
#'Input is two vectors of the same length, one being a "time" vector, and the other your corresponding y value vector
#'
#'@param time.vector vectored list of time values
#'@param y.vector a list of y value datapoints corresponding to the intervals of your time values.
#'@return Unit of output is the same as the unit of your y axis. for example, eAUC of an OD600 curve would have a unit "OD600"
#'
#'@export
eAUC <- function(time.vector,y.vector){
  if(length(time.vector)==length(y.vector)){
    
  x <- time.vector
  n <- length(x)
  y <- y.vector
  eAUC <- sum((x[2:n] - x[1:n-1]) * (y[2:n] + y[1:n-1]) /  2)
  
  return(eAUC)
  }else{
    print("vectors have different lengths! :-( :-(  ")
  }
}
