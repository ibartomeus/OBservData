
# This code uses the variable file_OK generated in processing_file.R
# (section 3 Save verified template)
# The code add a new line to "News.MD" when a new template is added to the
# database and updates the version number from `X.X.X` to `X.X+1.X`
# NOTE: last digit in version refers only to updates in software

news_lines <- readLines("News.md")
last_version <- str_match(news_lines[length(news_lines)], "`\\s*(.*?)\\s*`")
last_version <- strsplit(last_version[2],".", fixed = TRUE)
last_version <- as.numeric(last_version[[1]])
news_lines <- c(news_lines,
                paste0("* version `",last_version[1],".",(last_version[2]+1),".",
                last_version[3],
                "`: Addtion of ",file_OK," (",Sys.Date(),")"))

write_lines(news_lines,"News.MD")
