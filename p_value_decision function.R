p_value_decision <- function(p_value, true_hypothesis, threshold=0.05){
  
  if (true_hypothesis==0){
    ifelse(p_value < threshold, return(1),return(0)) # if p<.05 while H0 holds give 1 (Type I Error)
  }
  
  if (true_hypothesis==1){
    ifelse(p_value > threshold, return(1),return(0)) #if p>.05 while H1 holds, give 1 (Type II Error)
  }
}
