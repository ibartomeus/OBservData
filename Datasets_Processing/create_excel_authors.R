
create_excel_authors <- function(authors,i,studies,base_author_i,excel_authors){
  
  
  excel_author_i <- excel_authors %>% filter(`Study ID` %in% studies$code)
  excel_author_i[,7:8][excel_author_i[,7:8]==F] <- "YES/NO"
  
  
  # create a new workbook for outputs
  #++++++++++++++++++++++++++++++++++++
  # possible values for type are : "xls" and "xlsx"
  
  wb <- createWorkbook(type="xlsx")
  
  # Define some cell styles
  #++++++++++++++++++++++++++++++++++++
  # Title and sub title styles
  TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=16, 
                                     color="blue", isBold=TRUE, underline=1)
  SUB_TITLE_STYLE <- CellStyle(wb) + 
    Font(wb,  heightInPoints=14,
         isItalic=TRUE, isBold=FALSE)
  # Styles for the data table row/column names
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"), 
           pen=c("BORDER_THIN", "BORDER_THICK")) 
  # Create a new sheet in the workbook
  #++++++++++++++++++++++++++++++++++++
  sheet <- createSheet(wb, sheetName = "Author_data")
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to 
  #contain the title
  # - title : the text to use as title
  # - titleStyle : style object to use for title
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  # Add title and sub title into a worksheet
  #++++++++++++++++++++++++++++++++++++
  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title="Data ownership of your datasets",
                titleStyle = TITLE_STYLE)
  # Add sub title
  xlsx.addTitle(sheet, rowIndex=2, 
                title=authors[i],
                titleStyle = SUB_TITLE_STYLE)
  # Add a table into a worksheet
  #++++++++++++++++++++++++++++++++++++
  addDataFrame(excel_author_i, sheet, startRow=3, startColumn=1,row.names = F, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  # Change column width
  setColumnWidth(sheet, colIndex=1, colWidth=50)
  setColumnWidth(sheet, colIndex=2, colWidth=20)
  setColumnWidth(sheet, colIndex=c(3:6), colWidth=40)
  setColumnWidth(sheet, colIndex=7, colWidth=45)
  setColumnWidth(sheet, colIndex=8, colWidth=55)
  # Add a plot into a worksheet
  #++++++++++++++++++++++++++++++++++++
  
  # Save the workbook to a file...
  #++++++++++++++++++++++++++++++++++++
  
  
  file_name <- paste0("Data ownership of your datasets (",authors[i],").xlsx")
  file_name <- paste(base_author_i,file_name,sep = "/")
  
  saveWorkbook(wb, file_name)
  
  #write.xlsx(excel_author_i, file=file_name, 
  #           sheetName="Author_data")
  
}