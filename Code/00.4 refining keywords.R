keywords_matrix = sapply(myDict, FUN = function(y) {grepl(patter = y, x = myDict)}  )
diag(keywords_matrix) = NA
keywords_matrix = apply(keywords_matrix, 2, which)
keywords_matrix = unlist(keywords_matrix) %>%unique
keywords = myDict[-keywords_matrix]
rm(keywords_matrix)