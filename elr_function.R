

elr_function <- function(data=data){
  tryCatch(
  #Try this
    {
  elrmod <- effectLite(
    data = data,
    y = "y",
    x = "x",
    k = "k",
    z = "z",
    method = "sem",
    mimic="lm",             # since we use sem but mimic lm, we need to change other arguments default setting according to lm function
    fixed.cell=TRUE,        # Mit sem Methode wäre FALSE default
    fixed.z=TRUE,           # Mit sem Methode wäre FALSE deafult
    homoscedasticity=TRUE,  # Mit sem Methode wäre FALSE deafult
    test.stat = "Wald"
                    )
  return(elrmod)
    },
  #if error occurs:
    error=function(e){
    print(e)
  }
  
          )
}