#Run SS in Parallel
#Functions in assessment_helper_functions

#' Run SS in parallel
#'
#' across a number of cores
#' @param ncores Number of cores; default to 8
#' @param inds Indices of parallel runs; applies only to jitters
#' @param syscall Point to desired version of SS
#' @param dir Directory of jitter runs
#' @param readres Read in jitter results if TRUE, don't if FALSE
#' @author Peter Kuriyama
#' @export

run_ss_parallel <- function(ncores = 8, inds = 1:8, 
                            syscall, dir, 
                            # seed = 300, randruns = TRUE, 
                            readres = TRUE){
# browser()
  orig_dir <- getwd()
  start_time <- Sys.time()
  # set.seed(seed)
  # if(randruns) ntimes <- sample(1:10, length(inds))
  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  empty_res <- foreach::foreach(yy = inds, .packages = c("r4ss")) %dopar% {
    to <- paste0(dir, yy)
    setwd(to)
    # if(randruns){
    #   for(zz in 1:ntimes[yy]){
    #     temp_call <- system(syscall, intern = TRUE)   
    #   }
    # } else {
      temp_call <- system(syscall, intern = TRUE)  
    # }
    
    outs <- SS_output(dir = getwd(), printstats = FALSE, covar = FALSE)
    # if(readres == TRUE){
    #   outs <- SS_output(dir = getwd(), printstats = FALSE, covar = FALSE)  
    #   return(outs)
    # }
    setwd(orig_dir)
    return(outs)
  }
  # setwd(orig_dir)
  
  stopCluster(cl)
  run_time <- Sys.time() - start_time; run_time
  
  outout <- SSsummarize(empty_res)
  return(outout)
}

#Run SS in Parallel
#Functions in assessment_helper_functions

#' Read SS parallel
#'
#' across a number of cores
#' @param ncores Number of cores; default to 8
#' @param inds Indices of parallel runs; applies only to jitters
#' @param syscall Point to desired version of SS
#' @param dir Directory of jitter runs
#' @param readres Read in jitter results if TRUE, don't if FALSE
#' @author Peter Kuriyama
#' @export
ssoutput_parallel <- function(ncores = 8, folders){
# browser()
  start_time <- Sys.time()
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  empty_res <- foreach::foreach(ii = 1:length(folders), .packages = c("r4ss")) %dopar% {
    outs <- SS_output( folders[ii], printstats = FALSE)    
    return(outs)
  }
  
  stopCluster(cl)
  run_time <- Sys.time() - start_time; print(run_time)
  names(empty_res) <- folders
  return(empty_res)
  
}
