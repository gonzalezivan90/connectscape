library(dismo)
library(raster)
library(rgdal)

setwd('N:/Mi unidad/connectivity-nasa-USFSIP/01_original-data/Sabah_example_CDPOP+UNICOR/')
r <- raster('resist_base_roads.rsg')
plot(r)

rp <- randomPoints(r, 500)
class(rp)
dim(rp)
points(rp)
colnames(rp) <- c('X', 'Y')
head(rp)
write.csv(rp, 'sabah_500.xy', row.names = FALSE, quote = FALSE)
write.csv(rp[sample(1:100, 10), ], 'sabah_10.xy', row.names = FALSE, quote = FALSE)
write.csv(rp[sample(1:100, 20), ], 'sabah_20.xy', row.names = FALSE, quote = FALSE)
write.csv(rp[sample(1:100, 50), ], 'sabah_50.xy', row.names = FALSE, quote = FALSE)
write.csv(rp, 'sabah_100.xy', row.names = FALSE, quote = FALSE)





r0 <- resample(rp)

## Resample 
options(scipen = 999)
# options(scipen = 9)

outDir <- 'C:/Users/Admin/dockerdata/UNICOR/unicor/'

for( x in 1:6){
  # x = 6
  # x <- 1:6
  
  side <- floor(sqrt((10^(x))))  
  (outname <- paste0(outDir, '/size',x,'_side', side, 'px_', 10^(x),'totpix.asc'))
  #(side <- 2000)
  #(outname <- paste0(outDir, '/size',x,'_side', side, 'px_', side^(2),'totpix.asc'))
  
  if( ! file.exists(outname)){
    
    rtemp <- raster(nrows = side, ncols = side, ext = extent(r))
    rtemp <- raster(nrows = side, ncols = side, resolution = min(res(rtemp)), ext = extent(r))
    rsam <- resample(r, rtemp)
    writeRaster(rsam, format = 'ascii', filename = outname, overwrite=TRUE)
  }
  
  (outname2 <- paste0(outDir, '/size',x,'_side', side, 'px_', 10^(x),'totpix.rsg'))
  #(outname2 <- paste0(outDir, '/size',x,'_side', side, 'px_', side^(2),'totpix.rsg'))
  
  if( !file.exists(outname2)){
    asc <- read.delim(outname, header = FALSE)
    head(asc, 6)
    asc[1:5, 1] <- gsub('[[:blank:]]', '\t', gsub(' $', '', tolower(asc[1:5, 1])))
    head(asc, 6)
    dim(asc)
    write.table(asc, file = outname2, col.names = FALSE, row.names = FALSE, quote = FALSE)
    
    #file.rename(outname, paste0(outDir, '/size',x,'_side', side, 'px_', 10^(x),'totpix.rsg'))
  }
  # plot(rsam)
}

dim(r)
raster::resample(r, )


#### Convert to tif & shp

prj <- paste0("+proj=aea +lat_0=-15 +lon_0=125 +lat_1=7 +lat_2=-32 +x_0=0 ",
              "+y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
#temp <- raster('../roads_sq_nodatam999.tif')

dataPath <- 'N:/Mi unidad/git/connecting-landscapes/performance-tests/inputs/'
setwd( dataPath )
temp <- raster('size7.tif')
(rsg <- list.files(path = dataPath, pattern = 'rsg$'))
sapply(rsg, function(x) { # x <- rsg[10]
  r <- raster(x)
  r@crs <- temp@crs
  (fileR <- gsub('_.+', '.tif', x))
  if(!file.exists(fileR)) { writeRaster(r, file = fileR) }
})

(xy <- list.files(path = dataPath, pattern = 'xy$'))
sapply(xy, function(x) { # x <- xy[6]
  print(x)
  s <- read.csv(x)
  head(s)
  s$id <- 1:nrow(s)
  s$x <- s$X
  s$y <- s$Y
  coordinates(s) =~ x + y
  s@proj4string@projargs <- temp@crs@projargs
  fileS <- gsub('.xy', '.shp', x)
  if(!file.exists(fileS)) { writeOGR(s, dsn = '.', layer = gsub('.xy', '', x), driver = 'ESRI Shapefile') }
})






#### Create rip files

(rsg <- paste0('size',1:6,'_side', floor(sqrt((10^(1:6))))  , 'px_', 10^(1:6),'totpix.rsg'))

gd <- (expand.grid(xy = paste0('sabah_', c(10, 20, 50, 100),'.xy'), rsg = rsg))
gd$xy <- as.character(gd$xy)
gd$rsg <- as.character(gd$rsg)
gd$case <- LETTERS[1:nrow(gd)]
gd$rip <- paste0('case', gd$case, '.rip')
gd$run <- paste0('python UNICOR.py ', gd$rip)
write.csv(gd, 'cases_config_time.csv')

head(gd)
str(gd)

case0 <- read.delim('C:/Users/Admin/dockerdata/UNICOR/unicor/small_test.rip', header = FALSE, sep = '|')


setwd('C:/Users/Admin/dockerdata/UNICOR/unicor')

for( x in 1:nrow(gd)){ # x = 1
  case <- case0
  
  xy <- gd[x, 1]
  rs <- gd[x, 2]

  (caseName <- paste0(outDir, '/case_', LETTERS[x], '.rip'))
  (case[1, 1] <- gsub('small_test', paste0('case1', LETTERS[x]), case[1, 1]))
  (case[2, 1] <- gsub('small_test.rsg', rs, case[2, 1]))
  (case[3, 1] <- gsub('small_test_10pts.xy', xy, case[3, 1]))
  
  (case[21, 1] <- "Save_Path_Output    TRUE") # Save_Path_Output
  (case[24, 1] <- "Save_KDE_Output    TRUE") # Save_KDE_Output
  (case[25, 1] <- "Save_Category_Output    TRUE") # Save_Category_Output
  (case[26, 1] <- "Save_CDmatrix_Output    TRUE") # Save_CDmatrix_Output
  
  if(file.exists(xy) & file.exists(rs)){
    write.table(case, file = caseName, col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
}

outConf <- expand.grid(o1 = c(FALSE, TRUE), o2 = c(FALSE, TRUE), o3 = c(FALSE, TRUE), o4 = c(FALSE, TRUE))
colnames(outConf) <- c('Save_Path_Output', 'KDE_Output', 'Category_Output', 'CDmatrix_Output')

write.csv(outConf, 'caseO_output_config_time.csv')

(x <- which(gd$case == 'R'))
xy <- gd[x, 1]
rs <- gd[x, 2]
for( h in 1:nrow(outConf)){ # h = 1
  
  case <- case0 
  (caseName <- paste0(outDir, '/case',LETTERS[x],'_', h,'.rip'))
  print(caseName)
  (case[1, 1] <- gsub('small_test', paste0( 'case',LETTERS[x], '_', h ), case[1, 1]))
  (case[2, 1] <- gsub('small_test.rsg', rs, case[2, 1]))
  (case[3, 1] <- gsub('small_test_10pts.xy', xy, case[3, 1]))
  
  (case[21, 1] <- paste0("Save_Path_Output    ", outConf[h, 1])) # Save_Path_Output
  (case[24, 1] <- paste0("Save_KDE_Output    ", outConf[h, 2])) # Save_KDE_Output
  (case[25, 1] <- paste0("Save_Category_Output    ", outConf[h, 3])) # Save_Category_Output
  (case[26, 1] <- paste0("Save_CDmatrix_Output    ", outConf[h, 4])) # Save_CDmatrix_Output

  if(file.exists(xy) & file.exists(rs) ){
    write.table(case, file = caseName, col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
}



# cd %userprofile%\dockerdata\UNICOR\unicor
# python UNICOR.py caseA.rip

#for %a in (4 5 6 7 8 9 10 11 12 13 14 15) do python UNICOR.py caseR_%a.rip
#for /l %a in (1, 1, 16) do python UNICOR.py caseR_%a.rip

# >python UNICOR.py sabah.rip
# Log output directed to     : (sabah_test.log)
# The path list is empty, try increasing the path threshold, caclulating path as Nan
# Total UNICOR program run-time: 1:03:13.909943


#### Plot results
result <- read.csv('N:/Mi unidad/connectivity-nasa/01_original-data/Sabah_example_CDPOP+UNICOR/cases_config_time_A-X.csv')[1:24, ]
result$nXWpts <- as.numeric(gsub('[a-zA-Z]|\\.|_', '', result$xy))
result$npix <- gsub('size.+px_|tot.+', '', result$rsg)

library(ggplot2)
ggplot(result, aes(x = nXWpts, y = time, group = npix, color = npix)) + 
  geom_point() + geom_line() + 
  labs(title = 'Elapsed computing time in seconds', 
       x = 'Number of XW points', y = 'Time in seconds', color = 'Raster size in tot pixels') +
  theme(legend.position="bottom")
