
rm(list = ls())
# lists merged: 
# gender, small arm, statistics reduced

file1 <- "data/keywords/statistics_reduced_acronyms_en.txt"
file2 <- "data/keywords/statistics_reduced_acronyms_en_ja.txt"
file_output <- "data/keywords/statistics_reduced_acronyms_en_final.txt"
myDict = readLines(file1)
myDict = c(myDict, readLines(file2))
rm(file2, file1)
myDict = myDict %>% 
  trimws %>% 
  unique
keywords <- myDict


keywords_matrix = sapply(myDict, FUN = function(y) {grepl(patter = y, x = myDict)}  )
diag(keywords_matrix) = NA
keywords_matrix = apply(keywords_matrix, 2, which)
keywords_matrix = unlist(keywords_matrix) %>%unique
keywords = myDict[-keywords_matrix]
rm(keywords_matrix)

writeLines(keywords, file_output)

