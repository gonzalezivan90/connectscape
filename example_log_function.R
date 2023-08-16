

setwd('/home/shiny/')
fn <- '00_log_test.log'
write_log <- function(x = 1, fn){
  logInt <- c('Start\t0')
  writeLines(logInt, fn)
  Sys.sleep(10)
  
  logInt <- c(logInt, 'step1\t1')
  writeLines(logInt, fn)
  Sys.sleep(10)
  
  
  logInt <- c(logInt, 'step1\t2')
  writeLines(logInt, fn)
  Sys.sleep(10)
  
  logInt <- c(logInt, 'step1\t3')
  writeLines(logInt, fn)
  Sys.sleep(10)
}

write_log(x= 1, fn)
