fun_ordinalize <- function(x) {
  # Ensure the input is a numeric vector
  if (!is.numeric(x)) stop("Input must be numeric")
  
  # Determine the suffix based on the last digit
  suffix <- ifelse(x %% 100 %in% c(11,12,13), "th", 
                   ifelse(x %% 10 == 1, "st", 
                          ifelse(x %% 10 == 2, "nd", 
                                 ifelse(x %% 10 == 3, "rd", "th"))))
  
  # Paste the number and suffix together
  paste0(x, suffix)
}
