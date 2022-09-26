#'@title Prepare to Export the Dataframe to An Excel Sheet.
#'
#'@description The function saves the dataframe as a tab and prepares it for output into an excel sheet with a predetermined format.
#'@param tabname  a string with the tab's name.
#'@param datastable  the dataframe that will be exported to Excel.
#'@param tablename a string containing the table label and title, which will appear as the first row
#'@param filename the name of the spreadsheet
#'@return a spreadsheet containing all of the exported tables
#'@import openxlsx
#'@examples
#'Dat <- survival::lung
#'results <- surv_uni_cat(Dat, "time", "status", "sex", report_index = TRUE)
#'wb <- openxlsx::createWorkbook()
#'wb <- p2excel_pre("survival_results",results,"Table 1. Overall Survival anlaysis",wb)
#'## Not run:
#'## saveWorkbook(wb, file = "os.xlsx", overwrite = TRUE)
#'## End(Not run)
#'@export
#'@name p2excel_pre

p2excel_pre <-function(tabname = "Default", datastable, tablename = "Default", filename){
  addWorksheet(filename,tabname)
  writeData(filename, tabname,data.frame( tablename), startRow=1,colNames=FALSE)
  hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="bold")
  writeData(filename,tabname, datastable, startRow=2,headerStyle = hs1)
  setColWidths(filename,tabname, cols = 1:(dim(datastable)[2]), widths = "auto")
  freezePane(filename,tabname,firstActiveRow = 3, firstCol = TRUE)
  return(filename)
}
