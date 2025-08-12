p_value_decision <- function(p_value, true_hypothesis, cutoff="regular"){
  
################################################################################
# Cut-off Rules
################################################################################
  if(cutoff=="regular"){
    threshold <- 0.05
  }
    
  if(cutoff=="unusual"){
    threshold <- 0.10
  }
  
################################################################################
# Decision making
################################################################################
  if (true_hypothesis==0){
    ifelse(p_value < threshold, return(1),return(0)) 
    # if p<.05 while H0 holds give 1 (Type I Error)
  }
  
  if (true_hypothesis==1){
    ifelse(p_value > threshold, return(1),return(0)) 
    #if p>.05 while H1 holds, give 1 (Type II Error)
  }
}
