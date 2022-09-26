#'@title Export the A Single Dataframe to An Excel Sheet
#'
#'@description The function saves a dataframe into an excel sheet with a predetermined format.
#'@param tabname  a string with the tab's name.
#'@param datastable  the dataframe that will be exported to Excel.
#'@param tablename a string containing the table label and title, which will appear as the first row
#'@param filename the name of the spreadsheet
#'@return a spreadsheet containing an exported tables
#'@export
#'@name p2excel

p2excel <-function(tabname = "Default", datastable, tablename = "Default", filename = "Default.xlsx"){
  wb <- createWorkbook()
  addWorksheet(wb,tabname)
  writeData(wb, tabname,data.frame( tablename), startRow=1,colNames=FALSE)
  hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="bold")
  writeData(wb,tabname, datastable, startRow=2,headerStyle = hs1)
  setColWidths(wb,tabname, cols = 1:(dim(datastable)[2]), widths = "auto")
  freezePane(wb,tabname,firstActiveRow = 3, firstCol = TRUE)
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}
