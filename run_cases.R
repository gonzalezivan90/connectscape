#### 
setwd('/data/temp/_tests/')
list.files()
py_crk <- paste0(
  '/home/shiny/anaconda3/envs/cdujlab/bin/python ',
  '/home/shiny/connecting-landscapes/src/crk.py ')

py_lcc <- paste0(
  '/home/shiny/anaconda3/envs/cdujlab/bin/python ',
  '/home/shiny/connecting-landscapes/src/lcc.py ')


###### CAROL 1 
cmd1_crk <- paste0(py_crk, 
                   '/data/temp/_tests/GE_14500P.csv ',
                   '/data/temp/_tests/Carol-GE_uge_abscb_rf_res325_SQPIX1k.tif ',
                   '/data/temp/_tests/out_GECarol1km_crk_25k-l-1-1.tif ',
                   '25000 linear 1 1 None')

cmd1_lcc <- paste0(py_lcc, 
                  '/data/temp/_tests/GE_14500P.csv ',
                  '/data/temp/_tests/Carol-GE_uge_abscb_rf_res325_SQPIX1k.tif ',
                  '/data/temp/_tests/GE_14500/out_test_lcc.tif ',
                  '25000 1000 1')

paste0('taskset --cpu-list 6 /usr/bin/time -v ',
       cmd1_crk, ' &> /data/temp/_tests/log_CRK_GEcarol1.txt &')

paste0('taskset --cpu-list 5 /usr/bin/time -v ',
       cmd1_lcc, ' &> /data/temp/_tests/log_LCC_GEcarol1.txt &')

GDALinfo('/data/temp/_tests/Carol-GE_uge_abscb_rf_res325_SQPIX1k.tif')


###### CAROL 2
cmd2_crk <- paste0(py_crk, 
                   '/data/temp/_tests/GE_14500P.csv ',
                   '/data/temp/_tests/Carol-GE_uge_rmax260_2km_SQPIX2k.tif ',
                   '/data/temp/_tests/out_GECarol2km_crk_25k-l-1-1.tif ',
                   '25000 linear 1 1 None')

cmd2_lcc <- paste0(py_lcc, 
                  '/data/temp/_tests/GE_14500P.csv ',
                  '/data/temp/_tests/Carol-GE_uge_rmax260_2km_SQPIX2k.tif ',
                  '/data/temp/_tests/out_GECarol2km_lcc_25k-1k.tif ',
                  '25000 1000 1')

paste0('taskset --cpu-list 7 /usr/bin/time -v ',
       cmd2_crk, ' &> /data/temp/_tests/log_CRK_GEcarol2.txt &')

paste0('taskset --cpu-list 8 /usr/bin/time -v ',
       cmd2_lcc, ' &> /data/temp/_tests/log_LCC_GEcarol2.txt &')

GDALinfo('/data/temp/_tests/Carol-GE_uge_rmax260_2km_SQPIX2k.tif')


###### ERIC 2 
cmd3_crk <- paste0(py_crk, 
                   '/data/temp/_tests/SEATigerConnectivity_SourcePoints_HIGH_7KM_WEFCOMMid_MC1_GEN1.csv ',
                   '/data/temp/_tests/SEATigerConnectivity_Resistance.tif ',
                   '/data/temp/_tests/out_seatiger_CRK.tif ',
                   '25000 linear 1 1 None')

cmd3_lcc <- paste0(py_lcc, 
                   '/data/temp/_tests/SEATigerConnectivity_SourcePoints_HIGH_7KM_WEFCOMMid_MC1_GEN1.csv ',
                   '/data/temp/_tests/SEATigerConnectivity_Resistance.tif ',
                   '/data/temp/_tests/out_seatiger_LCC.tif ',
                   '25000 1000 1')

paste0('taskset --cpu-list 7 /usr/bin/time -v ',
       cmd3_crk, ' &> /data/temp/_tests/log_CRK_seatiger.txt &')

paste0('taskset --cpu-list 8 /usr/bin/time -v ',
       cmd3_lcc, ' &> /data/temp/_tests/log_LCC_seatiger.txt &')


###### ERIC 1 
cmd4_crk <- paste0(py_crk, 
                   '/data/temp/_tests/DPKYConnectivity_SourcePoints.csv',
                   '/data/temp/_tests/DPKYConnectivity_Resistance_r07r.tif',
                   '/data/temp/_tests/out_DPKY_crk.tif ',
                   '25000 linear 1 1 None')

cmd4_lcc <- paste0(py_lcc, 
                   '/data/temp/_tests/DPKYConnectivity_SourcePoints.csv',
                   '/data/temp/_tests/DPKYConnectivity_Resistance_r07r.tif',
                   '/data/temp/_tests/out_DPKY_lcc.tif ',
                   '25000 1000 1')

paste0('taskset --cpu-list 7 /usr/bin/time -v ',
       cmd4_crk, ' &> /data/temp/_tests/log_CRK_DPKY &')

paste0('taskset --cpu-list 8 /usr/bin/time -v ',
       cmd4_lcc, ' &> /data/temp/_tests/log_LCC_DPKY.txt &')


###### Zaneta
cmd5_crk <- paste(py_crk, 
                   '/data/temp/_tests/SourcePointsLions.shp',
                   '/data/temp/_tests/kaza.tif',
                   '/data/temp/_tests/out_Kaza_crk.tif',
                   '25000 linear 1 1 None')

cmd5_lcc <- paste(py_lcc, 
                   '/data/temp/_tests/SourcePointsLions.shp',
                   '/data/temp/_tests/kaza.tif',
                   '/data/temp/_tests/out_Kaza_LCC.tif',
                   '25000 100 1000 1 None')

paste0('taskset --cpu-list 7 /usr/bin/time -v ',
       cmd5_crk, ' &> /data/temp/_tests/log_CRK_kaza.txt &')

paste0('taskset --cpu-list 6 /usr/bin/time -v ',
       cmd5_lcc, ' &> /data/temp/_tests/log_LCC_Kaza.txt &')


####### debug + check

library(rgdal)
library(raster)





setwd('/data/temp/PH2023100311442505file8513323368416/')
paste('/home/shiny/anaconda3/envs/cdujlab/bin/python', 
      '/home/shiny/connecting-landscapes/src/crk.py', 
      'sp_100.shp', 
      '/data/temp/YU2023091418003605file28f7630e0b7ad//in_crk_fixed_OA2023091418005105file28f7632f9ff83.tif',
      '/data/temp/YU2023091418003605file28f7630e0b7ad//out_crk_OA_IG.tif', 
      '25000 linear 1 1 None')



setwd('/data/temp/PH2023100311442505file8513323368416/')
r <- raster('out_lcc_VU2023100312162205file85133565495a1.tif')
plot(r)
r@crs@projargs

pts <- readOGR('sp10.shp')
plot(r)
plot(pts, add = TRUE)

plot(pts, axes = TRUE)

r@crs@projargs
pts@proj4string@projargs

rgdal::GDALinfo('in_crk_OA2023091418005105file28f7632f9ff83.tif')

ex <- raster::extract(r, pts)



