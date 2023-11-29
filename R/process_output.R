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
    temp <- temp %>% select(2, 3, 4, 5, 6, 7, 8) %>% filter(Era != "VIRG", Era != "INIT")
  })
  out <- ldply(out)  
  out$model <- out[, 1]
  out[, 1] <- NULL
  return(out)
}

#Pull age selectivity curves for plotting
#' Pull the time series values
#'
#' @param res List of results for evaluation
#' @export
pull_ageselex <- function(res, fact = "Asel"){
  out <- lapply(res, FUN = function(xx){
    temp <- xx$ageselex
    names(temp)[8:18]
    
    ncol(temp)
    
    temp <- temp %>% filter(Factor == fact) %>% select(Fleet, Yr, Seas, 8:18) %>% 
      melt(id.var = c("Fleet", "Yr", "Seas"))
    temp$age <- as.numeric(as.character(temp$variable))
    temp$variable <- NULL
    return(temp)
  })
  out <- ldply(out)
  out$model <- out[, 1]
  out[, 1] <- NULL
  return(out)
  
}
