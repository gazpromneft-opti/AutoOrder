cData <- function() {

  ns <- list()
  
  # Set it to environment 
  # and set the name as cData
  
  ns <- list2env(ns)
  class(ns) <- "cData"
  return(ns)  
  
}
