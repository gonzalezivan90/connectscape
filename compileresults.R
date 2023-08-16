dataPath <- '/home/shiny/data/'; dir.create(dataPath)
unicorPath <- '/home/shiny/UNICOR/unicor/'; list.files(path = unicorPath)
gitPath <- '/home/shiny/connecting-landscapes/performance-tests/inputs/'; list.files(path = gitPath)
setwd(dataPath); list.files(path = dataPath)


ag <- read.csv( paste0(gitPath, 'scenarios_cola-unicor_42.csv') )
#View(ag)
{
  
ag$timeCOLAlcc <- NA
ag$timeUNIClcc <- NA
ag$timeCOLAcrk <- NA
ag$timeUNICcrk <- NA
ag$timeCOLAmat <- NA
ag$timeUNICmat <- NA

ag$ramCOLAlcc <- NA
ag$ramUNIClcc <- NA
ag$ramCOLAcrk <- NA
ag$ramUNICcrk <- NA
ag$ramCOLAmat <- NA
ag$ramUNICmat <- NA
}

for(i in 1:nrow(ag)){ # i = 6 }
  
  # i = which(ag$scen == 'K')
  case <- ag[i, ]
  
  (filesCola <- list.files(path = dataPath, pattern = case$scenario, full.names = TRUE))
  (filesUnic <- list.files(path = unicorPath, full.names = TRUE,
                          pattern = gsub('.rsg|.xy', '', paste0(case$rsg,'.+_', case$xy,'\\.'))
                          ))
  
  
  ### LCC
  if ( file.exists(case$lcc_cola_out) & file.exists(case$lcc_cola_log) ){
    #file.show(case$lcc_cola_log)
    (logf <- gsub('\t', '', readLines(case$lcc_cola_log), fixed = TRUE))
    
    (ag$ramCOLAlcc[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                          grep('Maximum resident ', logf, value = TRUE))) )
    
    (ag$timeCOLAlcc[i] <- (gsub( 'User.+\\:',  '',
                                grep('User time ', logf, value = TRUE))) )
    
  }
  
  
  if ( file.exists(case$lcc_unic_log) ){ # file.exists( paste0(case$lcc_unic_out, '.txt')) &
    (logf <- gsub('\t', '', readLines(case$lcc_unic_log), fixed = TRUE))
    if(!length(logf) == 0){
      
      
      # file.show(case$lcc_unic_log)
      
      (ag$ramUNIClcc[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                            grep('Maximum resident ', logf, value = TRUE))) )
      
      (ag$timeUNIClcc[i] <- (gsub( 'User.+\\:',  '',
                                   grep('User time ', logf, value = TRUE))) )
      
    }
  }
  
  
  ### CRK 
  
  if ( file.exists(case$crk_cola_out) & file.exists(case$crk_cola_log) ){
    #file.show(case$crk_cola_log)
    (logf <- gsub('\t', '', readLines(case$crk_cola_log), fixed = TRUE))
    
    (ag$ramCOLAcrk[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                         grep('Maximum resident ', logf, value = TRUE))) )
    
    (ag$timeCOLAcrk[i] <- (gsub( 'User.+\\:',  '',
                                grep('User time ', logf, value = TRUE))) )
    
  }
  
  
  if ( file.exists(case$crk_unic_log) ){ # file.exists(case$crk_unic_out) & 
    (logf <- gsub('\t', '', readLines(case$crk_unic_log), fixed = TRUE))
    
    if(!length(logf) == 0){
      
    
    (ag$ramUNICcrk[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                          grep('Maximum resident ', logf, value = TRUE))) )
    
    (ag$timeUNICcrk[i] <- (gsub( 'User.+\\:',  '',
                                 grep('User time ', logf, value = TRUE))) )
    }
  }
  

  
  ### CDMAT
  
  if ( file.exists(case$mat_cola_out) & file.exists(case$mat_cola_log) ){
    #file.show(case$mat_cola_log)
    (logf <- gsub('\t', '', readLines(case$mat_cola_log), fixed = TRUE))
    
    (ag$ramCOLAmat[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                         grep('Maximum resident ', logf, value = TRUE))) )
    
    (ag$timeCOLAmat[i] <- (gsub( 'User.+\\:',  '',
                                grep('User time ', logf, value = TRUE))) )
    
  }
  
  
  if (  file.exists(case$mat_unic_log) ){ # file.exists(case$mat_unic_out) &
    logf <- gsub('\t', '', readLines(case$mat_unic_log), fixed = TRUE)
    
    (ag$ramUNICmat[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                          grep('Maximum resident ', logf, value = TRUE))) )
    
    (ag$timeUNICmat[i] <- (gsub( 'User.+\\:',  '',
                                 grep('User time ', logf, value = TRUE))) )
    
  }
  
  ag[i, grep('scenario|time|ram', colnames(ag), value = TRUE)]

}


napos <- unique(unlist(sapply(ag, function(x) which(is.na(x)))))
length(napos)
ramdf <- ag[, grep('ram|cmd', colnames(ag), value = TRUE)]
head(ramdf)
barRam <- unique(unlist(sapply(ramdf, function(x) which(as.numeric(x) < 84000))))



repcmd <- c(ag$lcc_cola_cmd[!file.exists(paste0(dataPath, ag$lcc_cola_out))],
            ag$crk_cola_cmd[!file.exists(paste0(dataPath, ag$crk_cola_out))],
            ag$mat_cola_cmd[!file.exists(paste0(dataPath, ag$mat_cola_out))],
            
            ag$lcc_unic_cmd[!file.exists(paste0(unicorPath, ag$lcc_unic_out, '.kdepaths'))],
            ag$crk_unic_cmd[!file.exists(paste0(unicorPath, ag$crk_unic_out, '.kdepaths'))],
            ag$mat_unic_cmd[!file.exists(paste0(unicorPath, ag$mat_unic_out, '.cdmatrix.csv'))]
            )
length(repcmd)
repcmd <- c('### chmod +x /home/shiny/connectscape/bash_v2.sh    ## Run: /home/shiny/connectscape/bash_v2.sh', 
            'cd /home/shiny/UNICOR/unicor/',
            '',
            repcmd)

writeLines(repcmd, '/home/shiny/connectscape/bash_v2.sh')
file.show('/home/shiny/connectscape/bash_v2.sh')
system('chmod +x /home/shiny/connectscape/bash_v2.sh')
system('/home/shiny/connectscape/bashB.sh > outputfile_for_stdout &')
# /home/shiny/connectscape/bashB.sh > outputfile_for_stdout & # yes

wb <- which(ag < 84000, arr.ind = TRUE )
for (u in 1:nrow(wb)){
  cn <- colnames(ag[wb[u, 1]])
}



ag[, grep('ram', colnames(ag), value = TRUE)]
84576
ag$somefail[napos] <- 1


head(ag[, grep('scenario|time|ram', colnames(ag), value = TRUE)])
head(ag[napos, grep('scenario|time|ram', colnames(ag), value = TRUE)])

j <- grep('time|ram', colnames(ag))
ag[j] <- sapply(ag[j], as.numeric)
str(ag)
cmd2fix

apply(array, margin, ...)


head(ag)
write.csv(ag, '/home/shiny/connectscape/results_times_ram.csv')
#file.show('/home/shiny/connectscape/results_times_ram.csv')

library(ggplot2)
library(reshape2)

grep('scenario|time|ram|rsgo|shp', colnames(ag), value = TRUE)

result <- reshape2::melt(ag[, grep('scenario|time|ram|rsgo|shp', colnames(ag), value = TRUE)],
                         id.vars =  c('rsgo','shp', 'scenario'))


result$npix <- gsub('si.+px_|tot.+', '', result$rsgo)
result$pixs <- gsub('..+side|px.+', '', result$rsgo)
result$npts <- gsub('sab.+_|.shp', '', result$shp)
result$var <- gsub('COL.+|UNIC.+', '', result$variable)
result$soft <- gsub('ram|time|mat|crk|lcc', '', result$variable)
result$meth <- gsub('ram|time|COLA|UNIC', '', result$variable)
head(result)
tail(result)

result$value <- as.numeric(result$value)
result$value[result$var == 'time'] <- result$value[result$var == 'time']/60
result$npix <- as.numeric(result$npix)
result$pixs <- as.numeric(result$pixs)
result$methpts <- paste0(result$meth,result$npts)
head(result)

## Time 

df <- na.omit(subset(result, var == 'time'))
ggplot(df, 
       aes(x = as.factor(pixs), y = value, #group = methpts, 
           group = interaction(meth, npts),
           color = npts, shape = meth, linetype = meth)) +
  geom_point(size = 2) + 
  geom_line(
    #aes(#linetype = interaction(meth, npts))
  ) +
  labs(title = 'Elapsed computing time in seconds',
       x = 'Number of sqrt(total pixels)', 
       y = 'Time in minutes', 
       color = 'Number of points',
       shape = 'Method') +
  #facet_wrap(~soft) +
  facet_grid(soft~meth) +
  theme(legend.position="bottom") + 
  guides(shape=guide_legend(nrow=2,byrow=TRUE), 
         linetype=guide_legend(nrow=2,byrow=TRUE))


## Full graph
ggplot(result, 
       aes(x = as.factor(pixs), y = value, #group = methpts, 
           group = interaction(meth, npts),
           color = npts, shape = meth, linetype = meth)) +
  geom_point(size = 2) + 
  geom_line(
    #aes(#linetype = interaction(meth, npts))
  ) +
  labs(title = 'Elapsed computing time in seconds',
       x = 'Number of sqrt(total pixels)', 
       y = 'Time in minutes  //  RAM in KB', 
       color = 'Number of points',
       shape = 'Method') +
  #facet_wrap(~soft) +
  facet_grid(var~soft, scales = 'free_y') +
  theme(legend.position="bottom") + 
  guides(shape=guide_legend(nrow=2,byrow=TRUE), 
         linetype=guide_legend(nrow=2,byrow=TRUE))




## Full graph
head(result)
ggplot(result, 
       aes(x = (pixs), y = value, #group = methpts, 
           group = interaction(meth, npts),
           color = npts, shape = meth, linetype = meth)) +
  geom_point(size = 2) + 
  geom_line(
    #aes(#linetype = interaction(meth, npts))
  ) +
  labs(title = 'Elapsed computing time in seconds',
       x = 'Number of sqrt(total pixels)', 
       y = 'Time in minutes  //  RAM in KB', 
       color = 'Number of points',
       shape = 'Method') +
  #facet_wrap(~soft) +
  facet_grid(var~soft, scales = 'free_y') +
  theme(legend.position="bottom") + 
  guides(shape=guide_legend(nrow=2,byrow=TRUE), 
         linetype=guide_legend(nrow=2,byrow=TRUE))


