setwd('/home/shiny/')
Sys.sleep(10)
write.csv(Sys.time(), file = gsub('[[:punct:]]| ', '', Sys.time()))


if(FALSE){
  system('R -e "source(\'/home/shiny/testR.R\')"')
  system('setsid R -e "source(\'/home/shiny/connectscape/performance_cola.R\')" >/dev/null 2>&1 < /dev/null &')
  #setsid R -e "source('/home/shiny/connectscape/performance_cola.R')" >/dev/null 2>&1 < /dev/null &
  # setsid /home/shiny/connectscape/bash.sh >/dev/null 2>&1 < /dev/null &
  2+2
}


# kill -9 94395

# zz <- file("all2.Rout", open = "wt")
# sink(zz)
# sink(zz, type = "message")
# try(log("a"))
# ## revert output back to the console -- only then access the file!
# sink()
# file.show("all2.Rout")


# /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_100.shp /home/shiny/data/size5.tif /home/shiny/data/out_lcc_scenarioZ.tif 100000 2 100 1 &> /home/shiny/data/AALogzCOLAlcc_scenarioZ.txt
