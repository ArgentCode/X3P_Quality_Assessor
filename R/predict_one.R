problemDamageForest = get(load("../Data/problemDamageForest.RData"))
problemFeatheringForest = get(load("../Data/problemFeatheringForest.RData"))
problemGoodForest = get(load("../Data/problemGoodForest.RData"))
problemHolesForest = get(load("../Data/problemHolesForest.RData"))
problemRotationForest = get(load("../Data/problemRotationForest.RData"))
qualityForest = get(load("../Data/TestForest2.RData"))

predict_one <- function(X3P) {
  df <- data.frame(assess_bottomempty_cropped = double(),
                   assess_col_na_cropped = double(),
                   assess_median_na_proportion = double(),
                   assess_middle_na_proportion = double(),
                   extract_na_cropped = double(),
                   assess_rotation = double()
  )
  df[1,] = c(assess_bottomempty_cropped(X3P), assess_col_na_cropped(X3P),
             assess_median_na_proportion(X3P), assess_middle_na_proportion(X3P),
             extract_na_cropped(X3P), assess_rotation(X3P))

  damage = predict(problemDamageForest, df[1,], type = 'prob')[,2]
  feathering = predict(problemFeatheringForest, df[1,], type = 'prob')[,2]
  good = predict(problemGoodForest, df[1,], type = 'prob')[,2]
  holes = predict(problemHolesForest, df[1,], type = 'prob')[,2]
  rotation = predict(problemRotationForest, df[1,], type = 'prob')[,2]
  problem = returnProblem(damage, feathering, good, holes, rotation)

  return(c(predict(qualityForest, df, type = 'prob')[,2] * 100, problem))
}
