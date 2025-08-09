bain_preference <- function(bain_obj, true_hypothesis){
  
  selected <- which.max(bain_obj$fit$PMPc) # Hypothesis mith most support
  
  if (true_hypothesis==0){
    ifelse(selected == length(bain_obj$fit$PMPc), return(0),return(1)) # if Hc gets most support give 0 (correct decision), else give 1 (Type I Error)
    # Hc is always the last element, so no matter how many hypotheses, with length() we always get the last element
  }
  
  if (true_hypothesis==1){
    ifelse(selected == length(bain_obj$fit$PMPc), return(1),return(0)) # if Hc gets most support give 1 (Type II error), else give 0 (correct decision)
  }
  
}

## add an ic.cutoff argument