source("code/boot.R")

keywords = readLines("data/statistics.txt")
keywords = tolower(keywords)
keywords = unique(keywords)
keywords_matrix = sapply(keywords, FUN = function(y) {grepl(patter = y, x = keywords)}  )

diag(keywords_matrix) = NA



keywords_matrix = apply(keywords_matrix, 2, which)
keywords_matrix = unlist(keywords_matrix) %>%unique


keywords = keywords[-keywords_matrix]


writeLines(keywords, "data/statistics_reduced.txt")
