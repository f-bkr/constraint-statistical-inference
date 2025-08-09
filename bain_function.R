

bain_function <- function(est_AdjMeans, hypothesis,n,VCOV_AdjMeans, true_hypothesis) {
  tryCatch(
    # try this
    {
  bain_AdjMeans <- bain(x = est_AdjMeans,
                      hypothesis = hypothesis,
                      n = n,
                      Sigma = list(VCOV_AdjMeans),
                      group_parameters = 3, # each group is described by 3 parameters: x,k,z
                      joint_parameters = 0) # number of shared parameters in "estimates" is 0

  bain_decision <- bain_preference(bain_AdjMeans, true_hypothesis)
    },
    #if error occurs:
    error=function(e){
      print(e)
    }
  )

}