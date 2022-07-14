#Process the output
#Functions to process output of CPS assessments

#' Pull the parameters and total likelihood
#'
#' @param res List of results for evaluation
#' @export

pull_pars_lls <- function(res){
  out <- lapply(res, FUN = function(xx){
    pars <- xx$parameters
    pars <- pars %>% select(Label, Value, Min, Max, Init, Gradient)
    row.names(pars) <- NULL
    
    pars$total_ll <- xx$likelihoods_used[1, 1]
    return(pars)
  })
  out <- ldply(out)  
  names(out)[1] <- 'id'
  return(out)
}


#' Pull the time series values
#'
#' @param res List of results for evaluation
#' @export

pull_timeseries <- function(res){
  out <- lapply(res, FUN = function(xx){
    temp <- xx$timeseries
    temp <- temp %>% select(2, 3, 4, 5, 6, 7, 8) %>% filter(Era != "FORE", 
                                                            Era != "VIRG", Era != "INIT")
  })
  out <- ldply(out)  
}