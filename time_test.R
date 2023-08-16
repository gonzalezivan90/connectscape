rv <- raster::raster(volcano)


library(raster)
library(peakRAM)
plot(rv)


p0 <- peakRAM({
  t.start <- proc.time()
  resam <- (rv * 2)
  t.end <- proc.time()
  x0 <- t.end - t.start
})





r <- raster('/srv/shiny-server/roads_sq_nodatam999.tif')
plot(r)

p1 <- peakRAM({
  t.start <- proc.time()
  resam <- ({aa <- sapply(1:100, function(y) y * r)})
  t.end <- proc.time()
  x1 <- t.end - t.start
})





list.files('/srv/shiny-server/')

###
{
  p2 <- peakRAM({
    t.start <- proc.time()
    system(paste0('/home/shiny/anaconda3/envs/cdujlab/bin/python ',
                  '/home/shiny/connecting-landscapes/src/lcc.py ',
                  '/srv/shiny-server/sabah100pts.shp ', 
                  '/srv/shiny-server/roads_sq_nodatam999.tif ',
                  '/srv/shiny-server/data/lcc_out_25k_5_5_fromR.tif 25000 5 5')
    )
    t.end <- proc.time()
    x2 <- t.end - t.start
  })
}


### 
{
  Rprof(tf <- "rprof_testB.log", memory.profiling=TRUE)
  {
    t.start <- proc.time()
    system(paste0('/home/shiny/anaconda3/envs/cdujlab/bin/python ',
                  '/home/shiny/connecting-landscapes/src/lcc.py ',
                  '/srv/shiny-server/sabah100pts.shp ', 
                  '/srv/shiny-server/roads_sq_nodatam999.tif ',
                  '/srv/shiny-server/data/lcc_out_25k_5_5_fromR.tif 25000 5 5')
    )
    t.end <- proc.time()
    x2 <- t.end - t.start
  } ## 3.3GB
  
  Rprof(NULL) 
  summaryRprof(tf)
}


###

## capture all the output to a file.
zz <- file("all2.Rout", open = "wt")
sink(zz)
sink(zz, type = "output")
## revert output back to the console -- only then access the file!


cmd <- paste0('/usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python ',
              '/home/shiny/connecting-landscapes/src/lcc.py ',
              '/srv/shiny-server/sabah100pts.shp ', 
              '/srv/shiny-server/roads_sq_nodatam999.tif ',
              '/srv/shiny-server/data/lcc_out_25k_5_5_fromR.tif 25000 5 5 &> /home/shiny/out_time_x.txt')


ram <- capture.output(
  {s <- system(intern = TRUE, 
               gnore.stdout = FALSE, 
               ignore.stderr = FALSE,
               cmd
  )})

sink()
sink(type = "message")
file.show("all2.Rout")

read.delim('sink.a')
#/usr/bin/time -v <program> <args>
  
  
  read.('/home/shiny/timevout.txt')
file.show("/home/shiny/timevout.txt")
file.show("/home/shiny/out_time_x.txt")

file.show("/home/shiny/UNICOR/unicor/sma")




cmd <- paste0('/usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py ',
              'lcc_scenarioY.rip &> /home/shiny/data/A0.txt')
ram <- capture.output(
  {s <- system(intern = FALSE, 
               ignore.stdout = FALSE, 
               ignore.stderr = FALSE,
               cmd
  )})
