
# This code uses the variable file_OK generated in processing_file.R
# (section 3 Save verified template)

news_lines <- readLines("News.md")
last_version <- str_match(news_lines[length(news_lines)], "`\\s*(.*?)\\s*`")
last_version <- as.numeric(last_version[2])
news_lines <- c(news_lines,
                paste0("* version `",(last_version+0.01),
                "`: Addtion of ",file_OK," (",Sys.Date(),")"))

write_lines(news_lines,"News.MD")
write_lines(as.character(last_version+0.01),"version.txt")
