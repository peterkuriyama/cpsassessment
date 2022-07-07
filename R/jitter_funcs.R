#Jitter functions

#' Run jitters in parallel 
#'
#' Run SS jitter runs with specified number of cores
#' @param thedir Directory containing the jitter runs
#' @param thecores Number of cores to run in parallel
#' @param jitinds Number of jitter runs
#' @param thejitamount Percentage to jitter runs; default is 0.05 or 5 percent.
#' @param SScall Directory of Stock Synthesis Call, make sure it has -nohess
#' @author Peter Kuriyama
#' @export

rr_jitter_parallel <- function(thedir = "00_caal/4a_cohort_seprofile/7_jitters/",
                               thecores, thejitinds = 1:8, thejitamount, SScall){
  
  copy_files_jitter(dir = thedir, jitinds = thejitinds,
                    jitamount = thejitamount)
  
  run_ss_parallel(dir = thedir, inds = thejitinds,
                  syscall = SScall, ncores = thecores,
                  readres = FALSE)
  
  jitouts <- run_ss_parallel(dir = thedir, inds = thejitinds,
                             syscall = SScall, ncores = thecores, 
                             # randruns = FALSE,
                             readres = TRUE)
  
  return(jitouts)
}

#' Copy files to initialize jitter
#'
#' Copy jitter files, directory should contain a folder "0" that has the base model
#' @param dir Directory containing the jitter runs
#' @param jitinds Number of jitter runs
#' @param jitamount Percentage to jitter runs; default is 0.05 or 5 percent.
#' @author Peter Kuriyama
#' @export


copy_files_jitter <- function(dir, jitinds, jitamount){
  fromdir <- paste0(dir, "/0")
  #Check that jitter is set up
  starter <- readLines(paste0(dir, "/0/starter.ss"))
  tempstart <- strsplit(starter[grep('jitter', starter)  ], split = " ")[[1]]
  tempstart[1]  <- jitamount
  
  starter[grep('jitter', starter)  ] <- paste(tempstart, collapse = " ")
  
  #---------------------------Copy files over
  from <- paste0(dir, 0)
  for(ii in jitinds){
    to <- paste0(dir, ii)
    dir.create(to, showWarnings = FALSE)
    
    #Copy files
    file.copy(paste0(from, "/data.ss"), paste0(to, '/data.ss'))
    file.copy(paste0(from, "/control.ss_new"), paste0(to, '/control.ss_new'))
    writeLines(starter, paste0(to, '/starter.ss'))
    file.copy(paste0(from, "/forecast.ss"), paste0(to, '/forecast.ss'))  
    file.copy(paste0(from, "/ss.par"), paste0(to, '/ss.par'))  
  }
}