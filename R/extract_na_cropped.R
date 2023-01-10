extract_na_cropped <- function(x3p) {
  stopifnot(class(x3p) == "x3p")
  x3p = crop_X3P(x3p)

  dims <- dim(x3p$surface.matrix)
  nas <- sum(is.na(x3p$surface.matrix))

  return(nas/prod(dims)*100)
}
