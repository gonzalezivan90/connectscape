library(ggplot2)
setwd('N:/Mi unidad/git/connecting-landscapes/performance-tests/logs_scenarios-A_AP')

files <- data.frame(orig = list.files(pattern = 'scenario[A-Z].txt|scenario[A-Z][A-Z].txt'))

files$soft <- gsub('log|_.+', '',  files$orig)
files$func <- gsub('.+LA_|.+IC_|_s.+', '',  files$orig)
files$scen <- gsub('.+io|\\..+', '',  files$orig)
files$mins <- files$ramgb <- NA

table(files$scen, files$func)
(table(files$scen, files$func) !=2)
files[grep('AI', files$scen), ]


for(i in 1:nrow(files)){
  case <- files[i, ]
  (logf <- gsub('\t', '', readLines(case$orig), fixed = TRUE))
    if(!length(logf) == 0){
      # file.show(case$lcc_unic_log)
      
      (files$ramgb[i] <- as.numeric(gsub( 'Maximum.+\\:|[[:punct:]]|t',  '',
                                        grep('Maximum resident ', logf, value = TRUE))) /1000/1000)
      
      (sec <- gsub( 'User.+\\:',  '',
                     grep('User time ', logf, value = TRUE) ) )
      (files$mins[i] <- as.numeric(sec)/60)
      
    }
}




  
(rsg <- c("size1_side3px_10totpix", "size2_side10px_100totpix", "size3_side31px_1000totpix",
"size4_side100px_10000totpix", "size5_side316px_100000totpix", "size6_side1000px_1000000totpix",
"size7_side2000px_4000000totpix"))
xy <- c("10", "100", "20", "200", "50", "500")
head(files)

ag <- expand.grid(xy = xy, rsgo = rsg)
ag$scen <- c(LETTERS, paste0(LETTERS[1], LETTERS[1:(nrow(ag)-length(LETTERS))]))
ag$shp <- gsub(pattern = '[[:alpha:]]|[[:punct:]]', replacement = '', x = ag$xy)
ag$rsg <- gsub(pattern = '_.+', replacement = '', x = ag$rsgo)
ag$tif <- gsub(pattern = '_.+', replacement = '.tif', x = ag$rsgo)
ag$npix <- gsub('si.+px_|tot.+', '', ag$rsgo)
ag$spix <- gsub('..+side|px.+', '', ag$rsgo)
ag$size <- gsub('_.+', '', ag$rsgo)
ag$npts <- gsub('sab.+_|.shp', '', ag$xy)
ag$methpts <- paste0(ag$meth, ag$npts)


result0<- merge(files, ag, by  = 'scen')

head(result0)

result <- reshape2::melt(result0[, grep('orig|rsgo|shp|rsg|tif|xy|methpts', colnames(result0), invert = TRUE, value = TRUE)],
                         id.vars =  c('npix', 'spix', 'size', 'npts', 'soft', 'func', 'scen'))
head(result)

result$value <- as.numeric(result$value)
result$spix <- as.numeric(as.character(result$spix))
result$npix <- as.numeric(as.character(result$npix))
result$npts <- as.numeric(as.character(result$npts))
result$var <- ''
result$var[result$variable == 'mins'] <- 'CPU time (minutes)'
result$var[result$variable == 'ramgb'] <- 'RAM (GB)'

head(result)


ggplot(subset(result), #, func == 'mat' & variable == 'ramgb'), 
       aes(x = as.factor(spix),  
           y = (value), #group = methpts, 
           group = interaction(soft, npts),
           color = as.factor(npts), 
           shape = soft, linetype = soft)) +
  geom_point(size = 2) + geom_line(
    #aes(#linetype = interaction(meth, npts))
  ) +
  labs(title = 'Elapsed computing time (minutes) and RAM (GB)',
       x = 'Number of sqrt(total) pixels', 
       y = 'RAM in GB  // Time in minutes', 
       color = 'Number of points',
       shape = 'Software', linetype = 'Software') +
  #facet_wrap(~soft) +
  facet_grid(var~func, scales = 'free_y') +
  theme(legend.position="bottom") + 
  guides(shape=guide_legend(nrow=1,byrow=TRUE), 
         linetype=guide_legend(nrow=1,byrow=TRUE))
#+ lims(y = c(0, 25))


ggplot(subset(result), #, func == 'mat' & variable == 'ramgb'), 
       aes(x = as.factor(npix),  
           y = (value), #group = methpts, 
           group = interaction(soft, npts),
           color = as.factor(npts), 
           shape = soft, linetype = soft)) +
  geom_point(size = 2) + geom_line(
    #aes(#linetype = interaction(meth, npts))
  ) +
  labs(title = 'Elapsed computing time (minutes) and RAM (GB)',
       x = 'Number of pixels', 
       y = 'Time in minutes  //  RAM in GB', 
       color = 'Number of points',
       shape = 'Software', linetype = 'Software') +
  #facet_wrap(~soft) +
  facet_grid(var~func, scales = 'free_y') +
  theme(legend.position="bottom") + 
  guides(shape=guide_legend(nrow=1,byrow=TRUE), 
         linetype=guide_legend(nrow=1,byrow=TRUE))



write.csv(result, 'N:/Mi unidad/git/connecting-landscapes/performance-tests/results_42scenarios.csv')
rescola <- read.csv('N:/Mi unidad/git/connecting-landscapes/performance-tests/results_42scenarios.csv')
head(rescola)
tail(rescola)
lm_ram <- lm(value ~ npix + npts + func + soft, data = rescola[rescola$var == 'RAM (GB)', ])
lm_min <- lm(value ~ npix + npts + func + soft, data = rescola[rescola$var == 'CPU time (minutes)', ])

predDF <- data.frame(npix = 20000000,
                     npts = 14500,
                     func = c('lcc', 'crk', 'mat'), soft = 'COLA')
cbind(predSce, 
      CPUh = predict(lm_min, predDF)/60, 
      RAM = predict(lm_ram, predDF), 
      deparse.level = 0)

unique(rescola$npts)
unique(rescola$npix)

subset(rescola, npix == 4000000 & func == 'lcc' &
         npts == 500  & soft == 'COLA')
       
predDF <- data.frame(npix = 4000000,
                     npts = 500,
                     func = c('lcc', 'crk', 'mat'), 
                     soft = 'COLA')
cbind(predDF, 
      CPUh = predict(lm_min, predDF), 
      RAM = predict(lm_ram, predDF), 
      deparse.level = 0)





(smalldat <- subset(rescola, func == 'lcc' & soft == 'COLA' & var == 'RAM (GB)'
                   & npts == 500
                   #& npix == 4000000
))
summary( lm(value ~ npix + npts , data = smalldat) )
plot(smalldat$npix, smalldat$value)

plot(smalldat[c('npix', 'value')], 
     col = smalldat$npts, 
     pch = smalldat$npts)

predDF <- data.frame(npix = 1e+05,
                     npts = 500,
                     func = c('lcc', 'crk', 'mat'), 
                     soft = 'COLA')
cbind(predDF, 
      CPUh = predict(lm_min, predDF), 
      RAM = predict(lm_ram, predDF), 
      RAM2 = predict(
        lm(value ~ npix + npts , data = smalldat),
                     subset(predDF, func == 'lcc') ) )

subset(smalldat, npix == 1e+05 & npts == 500)
