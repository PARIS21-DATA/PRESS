fun_testing_read <- function(enc, input_file, n_rows) {
  result <- try(read.csv(file = input_file,
                         fileEncoding = enc,
                         nrows = n_rows,
                         header=TRUE,
                         sep="|"))
  print(enc)
  which(codepages == enc) %>% print
  print_time_diff(start) %>% print
  result_dim <- c(nrow(result), ncol(result))
  return(result_dim)
}
