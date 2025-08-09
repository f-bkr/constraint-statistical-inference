goric_preference <- function(goric_obj, true_hypothesis) {
  
  # Find which model has highest weight
  selected <- which.max(goric_obj$result$gorica.weights)
  
  if (true_hypothesis == 0) {
  # Return 1 if H1 (index 1, incorrect decision: Type I Error) preferred, else 0 for H0 (index 2; correct decision)
    if (selected == 1) {
      return(1)
    } else if (selected == 2) {
      return(0)
    } else {
      stop("oops sth. went wrong")
      }
    }
    if (true_hypothesis == 1){
      # Return 0 if H1 (index 1; correct decision) preferred, else 1 for H0 (index 2; incorrect decision: Type II error)
      ifelse(selected == 1, return(0),return(1))
    }
}


## add an ic.cutoff argument with 0.5 and other cutoffs from literature