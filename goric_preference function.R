goric_preference <- function(goric_obj, true_hypothesis, cutoff = "regular") {
  
  
  weights <- goric_obj$result$gorica.weights

 # select supported hypothesis 
  
  if (cutoff=="regular") {
    # Find which model has highest weight
    selected <- which.max(goric_obj$result$gorica.weights)
  }
  
  if (cutoff =="unusual") {
    # cut off with 0.95 weight
    if (max(weights) >= 0.95) {
      selected <- which.max(weights)
    } else {
      selected <- NA  # no decision possible
    }
  }
  
  
  # evaluate selected hypothesis
  
  if (is.na(selected)) {
    return(1)  # treat as incorrect decision
  }
  
  if (true_hypothesis == 0) {
  # Return 1 if H1 (index 1, incorrect decision: Type I Error) preferred, else 0 for H0 (index 2; correct decision)
    if (selected == 1) {
      return(1)
    } else {
      return(0)
    } 
  }
  
  if (true_hypothesis == 1){
    # Return 0 if H1 (index 1; correct decision) preferred, else 1 for H0 (index 2; incorrect decision: Type II error)
    if (selected == 1) {
      return(0)
    } else {
      return(1)
    }
  }
  
}