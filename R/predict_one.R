qualityForest = get(load("../Data/Cropped_Binary_RF_Model.RData"))

predict_one <- function(X3P) {
  df <- data.frame(assess_percentile_na_proportion = double(),
                   assess_col_na_cropped = double(),
                   extract_na_cropped = double(),
                   assess_middle_na_proportion = double(),
                   assess_bottomempty_cropped = double(),
                   assess_median_na_proportion = double()
  )
  df[1,] = c(assess_percentile_na_proportion(X3P), assess_col_na_cropped(X3P),
             extract_na_cropped(X3P), assess_middle_na_proportion(X3P),
             assess_bottomempty_cropped(X3P), assess_median_na_proportion(X3P))

  return(predict(qualityForest, df, type = 'prob')[,2] * 100)
}
