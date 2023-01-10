problemDamageForest = get(load("../Data/problemDamageForest.RData"))
problemFeatheringForest = get(load("../Data/problemFeatheringForest.RData"))
problemGoodForest = get(load("../Data/problemGoodForest.RData"))
problemHolesForest = get(load("../Data/problemHolesForest.RData"))
problemRotationForest = get(load("../Data/problemRotationForest.RData"))

predict_problem <- function (x3p) {
  damage = predict(problemDamageForest, test, type = 'prob')[,2]
  feathering = predict(problemFeatheringForest, test, type = 'prob')[,2]
  good = predict(problemGoodForest, test, type = 'prob')[,2]
  holes = predict(problemHolesForest, test, type = 'prob')[,2]
  rotation = predict(problemRotationForest, test, type = 'prob')[,2]
  returnProblem(damage, feathering, good, holes, rotation)
}
