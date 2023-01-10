assess_bottomempty_cropped <- function(x3p, n_cutoff = .2){
  stopifnot(class(x3p) == "x3p")

  x3p = crop_X3P(x3p)
  dims <- dim(x3p$surface.matrix)
  rows <- extract_na_row(x3p)*dims[1]/100 # ordered from top of scan to bottom
  #browser()
  bottom <- rev(rows)[1:floor(n_cutoff* length(rows))] # take bottom thirty percent

  amountmissinginthirty <- sum(bottom)
  amountinthirty <- (prod(dims)*n_cutoff)

  percentageofmissinginthirty <- (amountmissinginthirty/amountinthirty)* 100

  return(percentageofmissinginthirty)
}
