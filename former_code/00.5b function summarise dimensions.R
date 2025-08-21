fun_summarise_dimension <- function(var) {
  dim_summary <- var %>%
    bind_rows %>%
    as.matrix

  dim_summary <- tibble(rows = dim_summary[1,],
                        cols = dim_summary[2,])

  dim_summary <- dim_summary %>%
    mutate(code = vec_possible)

  dim_summary %>%
    group_by(rows, cols) %>%
    summarise(cnt = n()) %>%
    print
  return(dim_summary)
}
