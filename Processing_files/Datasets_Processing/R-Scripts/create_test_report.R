

report_lines <- c("================","Automatic report","================","")

if (nrow(data.insect) > 0){
  report_lines <- c(report_lines,"  \n",report1,"  \n",report2,
                    "  \n",report3,"  \n",report4,"  \n",report5)
}else{
  report_lines <- c(report_lines,"  \n,",report1,"  \n",report2,"  \n",report5)
  }


# Generate pdf

cat(report_lines, sep="  \n", file = "Your_new_study/Test_Report.Rmd")
render("Your_new_study/Test_Report.Rmd", pdf_document(),encoding="WINDOWS-1252")
file.remove("Your_new_study/Test_Report.Rmd") #cleanup
file.remove("Your_new_study/Test_Report.tex") #cleanup
