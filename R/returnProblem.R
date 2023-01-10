returnProblem <- function(Damage, Feathering, Good, Holes, Rotation) {
  temp = max(Feathering, Good, Holes, Rotation)
  if (Damage >= temp) {
    return ("Damage")
  }

  temp = max(Damage, Good, Holes, Rotation)
  if (Feathering >= temp) {
    return ("Feathering")
  }

  temp = max(Damage, Feathering, Holes, Rotation)
  if (Good >= temp) {
    return ("Good")
  }

  temp = max(Damage, Feathering, Good, Rotation)
  if (Holes >= temp) {
    return ("Holes")
  }

  temp = max(Damage, Feathering, Good, Holes)
  if (Rotation >= temp) {
    return ("Rotation")
  }

}
