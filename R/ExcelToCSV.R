#'Convert Excel workbook into separate CSV files
#'
#'Separate Excel worksheets into separate .csv files. filenames will be your.sheetname.csv. it is important to make these sheet names exactly the names of your plates, corresponding to the PLATE names in your plate layout and plate blank files.
#'
#'@param path is path to a multi-sheet excel file
#'@param out_dir directory where you wish to send these new files, if null, it will send to current directory
#'@return multiple .csv files
#'@export
ExcelToCSV <- function(path, out_dir = NULL) {
  if (is.null(out_dir)) out_dir <- dirname(path)
  sheets <- readxl::excel_sheets(path)
  filenames <- file.path(out_dir, paste0(sheets, ".csv"))
  dats <- lapply(sheets, readxl::read_excel, path = path)
  lapply(seq_along(dats), function(i) write.csv(dats[[i]],filenames[i], row.names = FALSE)) #changed readr::write_csv to write.csv. added row.names = FALSE
  invisible()
}

#modded from janusvm @ stackoverflow  https://stackoverflow.com/questions/50238645/r-split-excel-workbook-into-csv-files

#x <- readxl::read_xlsx("C:/Users/Derek Debrauske/Desktop/Chemgen Valid R2/DD 96well 48hr_20210301_141045 -- R2.B.xlsx")

#path = "C:/Users/Derek Debrauske/Desktop/Chemgen Valid R2/DD 96well 48hr_20210301_141045 -- R2.B.xlsx"
