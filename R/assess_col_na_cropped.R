
assess_col_na_cropped <- function(x3p, perc_of_col = 20, threshold_prop = 0.2){
  stopifnot(class(x3p) == 'x3p')
  x3p = crop_X3P(x3p)
  dims <- dim(x3p$surface.matrix)
  col_na_perc <- extract_na_column(x3p)
  bad_cols <- sum(col_na_perc > perc_of_col)
  threshold <- dims[1] * threshold_prop

  return(bad_cols / threshold)
}
