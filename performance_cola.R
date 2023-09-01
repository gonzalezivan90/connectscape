dataPath <- '/home/shiny/debug/'; dir.create(dataPath)
setwd(dataPath); list.files(path = dataPath)
unicorPath <- '/home/shiny/UNICOR/unicor/'; list.files(path = unicorPath)
gitPath <- '/home/shiny/connecting-landscapes/performance-tests/inputs/'; list.files(path = gitPath)

# # cp /home/shiny/connecting-landscapes/performance-tests/inputs/* /home/shiny/UNICOR/unicor/
# # cp /home/shiny/connecting-landscapes/performance-tests/inputs/* /home/shiny/data/

library(raster)


{ ### FIX origfiles
  # ## CHANGE -dstnodata "-9999"
  # setwd(dataPath)
  # (tifs <- list.files(pattern = '*5.tif'))
  # sapply(tifs, function(x) {file.rename(x, gsub('^size', 'orig_size', x))})
  # (tifs <- list.files(pattern = 'orig.+\\.tif'))
  # for(j in 1:length(tifs)){ # j = 1
  #   system(
  #     paste0('gdalwarp ', tifs[j], " ", gsub('orig_', '', tifs[j]),' -dstnodata "-9999"')
  #   )
  # }
  # 
  # ## File copy
  # setwd(unicorPath)
  # (rsgs <- list.files(pattern = '^size[0-9].rsg'))
  # sapply(rsgs, function(x) { # x = tifs[1]
  #   file.copy(x, gsub('\\.rsg', 'lcc.rsg', x))
  #   file.copy(x, gsub('\\.rsg', 'crk.rsg', x))
  #   file.copy(x, gsub('\\.rsg', 'mat.rsg', x))
  # })
}


### 
{
  RIP <- c('Session_label    SESSIONLABEL',
           'Grid_Filename    RSGFILE',
           'XY_Filename    XYFILE',
           'Use_Direction	FALSE',
           'Type_Direction	TYPEDIRECTION',
           'Use_Resistance	USERESISTANCE',
           'Barrier_or_U_Filename	BARRIER',
           'Direction_or_V_Filename	DIRECTION',
           'Speed_To_Resistance_Scale	SPEED',
           'Use_ED_threshold    USEEDTHRESHOLD',
           'ED_Distance   EDDISTANCE',
           'Edge_Type	EDGETYPE',
           'Transform_function	TRANSFUNCTION',
           'Const_kernal_vol	CONSTKERNALVOL',
           'Kernel_volume   KERNELVOLUME',
           'Edge_Distance    EDGEDISTANCE',
           'Number_of_Processes    NPROCESSES',
           'KDE_Function    KDEFUNCTION',
           'KDE_GridSize    KDEGRIDSIZE',
           'Number_of_Categories    NOFCATEGORIES',
           'Save_Path_Output    SAVEPATHOUTPUT',
           'Save_IndividualPaths_Output    SAVEINDIVIDUALPATHSOUTPUTS',
           'Save_GraphMetrics_Output    SAVEGRAPHS',
           'Save_KDE_Output    SAVEKDEOUTPUT',
           'Save_Category_Output    SAVECATEGORYOUTPUT',
           'Save_CDmatrix_Output    SAVECDMATRIXOUTPUT')
  
  RIP <- c('Session_label    SESSIONLABEL',                 #### 
           'Grid_Filename    RSGFILE',                 #### 
           'XY_Filename    XYFILE',                 #### 
           'Use_Direction	FALSE',
           'Type_Direction	FlowAcc',
           'Use_Resistance	TRUE',
           'Barrier_or_U_Filename	ompletebarr_test2.txt',
           'Direction_or_V_Filename	ompletebarr_test2.txt',
           'Speed_To_Resistance_Scale	0;10',
           'Use_ED_threshold    False',
           'ED_Distance   10000',
           'Edge_Type	EDGETYPE',                 #### 
           'Transform_function	TRANSFUNCTION', # TRANSFUNCTION
           'Const_kernal_vol	CONSTKERNALVOL',
           'Kernel_volume   KERNELVOLUME',
           'Edge_Distance    EDGEDISTANCE',                #### 
           'Number_of_Processes    1',                 #### 
           'KDE_Function    Gaussian', 
           'KDE_GridSize    KDEGRIDSIZE',                 #### 
           'Number_of_Categories    5',
           'Save_Path_Output    SAVEPATHOUTPUT',                   ####
           'Save_IndividualPaths_Output    FALSE',                  #### 
           'Save_GraphMetrics_Output    FALSE',                 #### 
           'Save_KDE_Output    SAVEKDEOUTPUT',                  #### 
           'Save_Category_Output    FALSE',
           'Save_CDmatrix_Output    SAVECDMATRIXOUTPUT')            #### 
  
  ripTemplate <- data.frame(RIP, row.names = gsub(' .+|\t.+', '', RIP) )
  #write.table(ripFile, file = 'caseName.rip', col.names = FALSE, row.names = FALSE, quote = FALSE)
}

options(scipen = 999)
params <- list(lcc4 = 100000, 
               lcc5 = 2, 
               lcc6 = 100, 
               crk4 = 1000000, # [4] distance threshold (should be in cost distance units (meters x resistance)*)
               crk5 = 'linear', # [5] kernel shape (linear, gaussian) NOTE: UNICOR doesn't have a gaussian option for CRK
               crk6 = 10000, # [6] kernel volume (kvol in crk.py), only active is Const_kernel_vol is FALSE
               mat4 = 100000, # [4] distance threshold (should be in cost distance units (meters x resistance)*)
               ncores = 1
)


(rsg <- list.files(pattern = 'pix.rsg'))
(xy <- list.files(pattern = 'sabah.+.xy'))

{
  ag <- expand.grid(xy = xy, rsgo = rsg)
  ag$shp <- gsub(pattern = 'xy', replacement = 'shp', x = ag$xy)
  ag$rsg <- gsub(pattern = '_.+', replacement = '.rsg', x = ag$rsgo)
  ag$tif <- gsub(pattern = '_.+', replacement = '.tif', x = ag$rsgo)
  head(ag, 20)
  
  
  ag$scen <- c(LETTERS, paste0(LETTERS[1], LETTERS[1:(nrow(ag)-length(LETTERS))]))
  ag$scenario <- paste0('scenario', ag$scen)
  head(ag)
  tail(ag)
  
  
  
  ## LCC 
  ag$lcc_unic_rip <- paste0('lcc_', ag$scenario, '.rip')
  ag$lcc_unic_tif <- paste0(tools::file_path_sans_ext(ag$rsg),'lcc.rsg')
  
  ag$lcc_unic_log <- paste0('logUNIC_lcc_', ag$scenario, '.txt')
  ag$lcc_cola_log <- paste0('logCOLA_lcc_', ag$scenario, '.txt')
  
  ag$lcc_unic_out <- paste0(tools::file_path_sans_ext(ag$rsg), 'lcc_', 
                            tools::file_path_sans_ext(ag$xy), '')
  # size5_side316px_100000totpix.rsg  + sabah_20.xy = size5_side316px_100000totpix_sabah_20.{EXT}
  # {EXT}: raster(.levels)  .kdepaths .addedpaths.txt  (csv) paths.csv .cdmatrix.csv
  
  ag$lcc_cola_out <- paste0('out_lcc_', ag$scenario, '.tif')
  ag$lcc_cola_out2 <- paste0('cola_lcc_', ag$scenario, '.tif')
  ag$lcc_unic_out2 <- gsub('out', 'unic', ag$lcc_cola_out)
  
  ag$lcc_unic_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v ', # cd /home/shiny/UNICOR/unicor/ && 
                            '/home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py ',
                            'lcc_', ag$scenario, '.rip',
                            ' &> ', dataPath, ag$lcc_unic_log)
  ag$lcc_cola_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python ',
                            '/home/shiny/connecting-landscapes/src/lcc.py ',
                            dataPath, ag$shp,' ', 
                            dataPath, ag$tif,' ', 
                            dataPath, ag$lcc_cola_out,
                            ' ', params$lcc4, # [4] distance threshold (should be in cost distance units (meters x resistance)*)
                            ' ', params$lcc5,  # [5] corridor smoothing factor (in number of cells)
                            ' ', params$lcc6,  # [6] corridor tolerance (in cost distance units)
                            ' 1 &> ', dataPath, ag$lcc_cola_log)
  
  
  
  ## CRK 
  
  ag$crk_unic_rip <- paste0('crk_', ag$scenario, '.rip')
  ag$crk_unic_tif <- paste0(tools::file_path_sans_ext(ag$rsg),'crk.rsg')
  
  ag$crk_unic_log <- paste0('logUNIC_crk_', ag$scenario, '.txt')
  ag$crk_cola_log <- paste0('logCOLA_crk_', ag$scenario, '.txt')
  
  ag$crk_unic_out <- paste0(tools::file_path_sans_ext(ag$rsg), 'crk_', 
                            tools::file_path_sans_ext(ag$xy), '')
  # size5_side316px_100000totpix.rsg  + sabah_20.xy = size5_side316px_100000totpix_sabah_20.{EXT}
  # {EXT}: raster(.levels)  .kdepaths .addedpaths.txt  (csv) paths.csv .cdmatrix.csv
  
  ag$crk_cola_out <- paste0('out_crk_', ag$scenario, '.tif')
  ag$crk_cola_out2 <- paste0('cola_crk_', ag$scenario, '.tif')
  ag$crk_unic_out2 <- gsub('out', 'unic', ag$crk_cola_out)
  
  
  ag$crk_unic_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v ', # cd /home/shiny/UNICOR/unicor/ && 
                            '/home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py ',
                            'crk_', ag$scenario, '.rip',
                            ' &> ', dataPath, ag$crk_unic_log)
  
  ag$crk_cola_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python ',
                            '/home/shiny/connecting-landscapes/src/crk.py ',
                            dataPath, ag$shp,' ', 
                            dataPath, ag$tif,' ', 
                            dataPath, ag$crk_cola_out,
                            ' ', params$crk4, # [4] distance threshold (should be in cost distance units (meters x resistance)*)
                            ' ', params$crk5,  # [5] kernel shape (linear, gaussian) NOTE: UNICOR doesn't have a gaussian option for CRK
                            ' ', params$crk6,  # [6] kernel volume (kvol in crk.py), only active is Const_kernel_vol is FALSE
                            ' 1 &> ', dataPath, ag$crk_cola_log)
  
  
  
  ## cmat 
  
  ag$mat_unic_rip <- paste0('mat_', ag$scenario, '.rip')
  ag$mat_unic_tif <- paste0(tools::file_path_sans_ext(ag$rsg),'mat.rsg')
  
  ag$mat_unic_log <- paste0('logUNIC_mat_', ag$scenario, '.txt')
  ag$mat_cola_log <- paste0('logCOLA_mat_', ag$scenario, '.txt')
  
  ag$mat_unic_out <- paste0(tools::file_path_sans_ext(ag$rsg), 'mat_', 
                            tools::file_path_sans_ext(ag$xy), '')
  # size5_side316px_100000totpix.rsg  + sabah_20.xy = size5_side316px_100000totpix_sabah_20.{EXT}
  # {EXT}: raster(.levels)  .kdepaths .addedpaths.txt  (csv) paths.csv .cdmatrix.csv
  
  ag$mat_cola_out <- paste0('out_mat_', ag$scenario, '.csv')
  ag$mat_cola_out2 <- paste0('cola_mat_', ag$scenario, '.csv')
  ag$mat_unic_out2 <- gsub('out', 'unic', ag$mat_cola_out)
  
  
  
  ag$mat_unic_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v ', #cd /home/shiny/UNICOR/unicor/ && 
                            '/home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py ',
                            'mat_', ag$scenario, '.rip',
                            ' &> ', dataPath, ag$mat_unic_log)
  
  ag$mat_cola_cmd <- paste0('taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python ',
                            '/home/shiny/connecting-landscapes/src/create_cdmat.py ',
                            dataPath, ag$shp,' ', 
                            dataPath, ag$tif,' ', 
                            dataPath, ag$mat_cola_out,
                            ' ', params$mat4,# [4] distance threshold (should be in cost distance units (meters x resistance)*)
                            ' 1 &> ', dataPath, ag$mat_cola_log)
  
}

# write.csv(ag, paste0(gitPath, 'scenarios_cola-unicor_', nrow(ag), '.csv'))
# write.csv(ag, paste0(dataPath, 'scenarios_cola-unicor_', nrow(ag), '.csv'))
# View(ag)

# sapply(
#   list.files(full.names = TRUE, path = dataPath, pattern = 'scenario'), 
#   file.remove
# )
# 
# sapply(
#   list.files(path = unicorPath, full.names = TRUE, 
#              pattern = 'scenario|addedpaths|kdepaths|levels|paths.csv|.cdmatrix.csv'),
#   file.remove
# )

ag[, 1:6]

for ( i in (1:nrow(ag))){ # i = 20; i = 22; i = which(ag$scenario == 'scenarioS')
  
  (case <- ag[i, ])
  
  print(case$scenario)
  
  cat(paste0(' \n\n\n\n\n\n\n ',
             '--------------------------------------------------------',
             i, case$scenario, ' \n\n\n\n\n\n\n ',
             '--------------------------------------------------------') )
  #sprintf(fmt = " %i - %s", i, case$scenario) # }
  
  {
    ## LCC
    if(!file.exists(case$lcc_unic_rip)){}
    
    rip_lcc <- ripTemplate
    
    rip_lcc$RIP <- gsub('SESSIONLABEL', paste0('lcc_', case$scenario), rip_lcc$RIP)
    rip_lcc$RIP <- gsub('RSGFILE', case$lcc_unic_tif, rip_lcc$RIP)                 #### 
    rip_lcc$RIP <- gsub('XYFILE',  case$xy, rip_lcc$RIP)               #### 
    
    rip_lcc$RIP <- gsub('EDGETYPE', 'threshold', rip_lcc$RIP)                 ####
    rip_lcc$RIP <- gsub('TRANSFUNCTION', params$crk5, rip_lcc$RIP) # TRANSFUNCTION
    rip_lcc$RIP <- gsub('CONSTKERNALVOL', FALSE, rip_lcc$RIP)
    rip_lcc$RIP <- gsub('KERNELVOLUME', params$crk6, rip_lcc$RIP)            #### 
    rip_lcc$RIP <- gsub('EDGEDISTANCE', params$lcc4, rip_lcc$RIP)            #### 
    rip_lcc$RIP <- gsub('KDEGRIDSIZE',  params$lcc5, rip_lcc$RIP)            ####  
    
    rip_lcc$RIP <- gsub('SAVEPATHOUTPUT', TRUE, rip_lcc$RIP)
    rip_lcc$RIP <- gsub('SAVEKDEOUTPUT', TRUE, rip_lcc$RIP)
    rip_lcc$RIP <- gsub('SAVECDMATRIXOUTPUT', FALSE, rip_lcc$RIP)
    
    write.table(rip_lcc, file = paste0('lcc_', case$scenario,'.rip'), col.names = FALSE, row.names = FALSE, quote = FALSE)
    write.table(rip_lcc, file = paste0('/home/shiny/UNICOR/unicor/lcc_', case$scenario,'.rip'), 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    
    
    ## CRK
    
    rip_crk <- ripTemplate
    
    rip_crk$RIP <- gsub('SESSIONLABEL', paste0('crk_', case$scenario), rip_crk$RIP)
    rip_crk$RIP <- gsub('RSGFILE', case$crk_unic_tif, rip_crk$RIP)                 #### 
    rip_crk$RIP <- gsub('XYFILE',  case$xy, rip_crk$RIP)               #### 
    
    rip_crk$RIP <- gsub('EDGETYPE', 'all_paths', rip_crk$RIP)                 #### 
    rip_crk$RIP <- gsub('TRANSFUNCTION', params$crk5, rip_crk$RIP) # TRANSFUNCTION
    rip_crk$RIP <- gsub('CONSTKERNALVOL', TRUE, rip_crk$RIP)
    rip_crk$RIP <- gsub('KERNELVOLUME', params$crk6, rip_crk$RIP)            #### 
    rip_crk$RIP <- gsub('EDGEDISTANCE', params$crk4, rip_crk$RIP)            #### 
    rip_crk$RIP <- gsub('KDEGRIDSIZE',  params$lcc5, rip_crk$RIP)            ####  
    
    rip_crk$RIP <- gsub('SAVEPATHOUTPUT', TRUE, rip_crk$RIP)
    rip_crk$RIP <- gsub('SAVEKDEOUTPUT', TRUE, rip_crk$RIP)
    rip_crk$RIP <- gsub('SAVECDMATRIXOUTPUT', FALSE, rip_crk$RIP)
    
    write.table(rip_crk, file = paste0('crk_', case$scenario,'.rip'), col.names = FALSE, row.names = FALSE, quote = FALSE)
    write.table(rip_crk, file = paste0('/home/shiny/UNICOR/unicor/crk_', case$scenario,'.rip'), 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    
    ## MAT
    rip_mat <- ripTemplate
    
    rip_mat$RIP <- gsub('SESSIONLABEL', paste0('mat_', case$scenario), rip_mat$RIP)
    
    rip_mat$RIP <- gsub('RSGFILE', case$mat_unic_tif, rip_mat$RIP)                 #### 
    rip_mat$RIP <- gsub('XYFILE',  case$xy, rip_mat$RIP)               #### 
    
    rip_mat$RIP <- gsub('EDGETYPE', 'threshold', rip_mat$RIP)                 #### 
    rip_mat$RIP <- gsub('TRANSFUNCTION', params$crk5, rip_mat$RIP) # TRANSFUNCTION
    rip_mat$RIP <- gsub('CONSTKERNALVOL', TRUE, rip_mat$RIP)
    rip_mat$RIP <- gsub('KERNELVOLUME', params$crk6, rip_mat$RIP)            #### 
    
    rip_mat$RIP <- gsub('EDGEDISTANCE', params$mat4, rip_mat$RIP)            #### 
    rip_mat$RIP <- gsub('KDEGRIDSIZE',  params$lcc5, rip_mat$RIP)            ####  
    
    rip_mat$RIP <- gsub('SAVEPATHOUTPUT', FALSE, rip_mat$RIP)
    rip_mat$RIP <- gsub('SAVEINDIVIDUALPATHSOUTPUTS', FALSE, rip_mat$RIP)
    rip_mat$RIP <- gsub('SAVEGRAPHS', FALSE, rip_mat$RIP)
    rip_mat$RIP <- gsub('SAVEKDEOUTPUT', FALSE, rip_mat$RIP)
    rip_mat$RIP <- gsub('SAVECDMATRIXOUTPUT', TRUE, rip_mat$RIP)
    
    write.table(rip_mat, file = paste0('mat_', case$scenario,'.rip'), col.names = FALSE, row.names = FALSE, quote = FALSE)
    write.table(rip_mat, file = paste0('/home/shiny/UNICOR/unicor/mat_', case$scenario, '.rip'), 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
}


# setwd('/home/ubuntu/data/')
setwd(unicorPath) # '/home/shiny/UNICOR/unicor/'
system('pwd')
for ( i in (1:nrow(ag))){ # i = 20; i = 22; i = which(ag$scenario == 'scenarioS')
  # i = 1
  (case <- ag[i, ])
  case$scenario
  
  ###
  
  if (FALSE) { ## Check parameters
    r <- raster(paste0(dataPath, case$tif))
    shp <- shapefile(paste0(dataPath, case$shp))
    plot(r); plot(shp, add = TRUE)
  }
  
  
  
  if (TRUE) {
    sinkLogName <- paste0(dataPath, case$scenario,'_sink.log'); # file.show(sinkLogName)
    sink(file = sinkLogName, type = c("output", "message"))
    
    ## LCC ----------------
    #size4lcc_sabah_100.addedpaths.txt  size4lcc_sabah_100.kdepaths
    if( !all ( 
      file.exists( 
        paste0(unicorPath, 
               case$lcc_unic_out, 
               c('.kdepaths', '.addedpaths.txt') )
      ) 
    ) ) {
      print('run lcc')
      system(case$lcc_unic_cmd, intern = TRUE); 
    }
    
    if( ! file.exists( paste0(dataPath, case$lcc_cola_out) ) ){
      system(case$lcc_cola_cmd, intern = TRUE); 
    }
    
    
    if ( FALSE) {
      # {EXT}: raster(.levels)  .kdepaths .addedpaths.txt  (csv) paths.csv .cdmatrix.csv
      ru1 <- raster(paste0(unicorPath, case$lcc_unic_out, '.kdepaths'));# plot(ru1)
      ru2 <- raster(paste0(unicorPath, case$lcc_unic_out, '.addedpaths.txt')); #plot(ru2)
      rc1 <- raster(paste0(dataPath, case$lcc_cola_out)); #plot(rc1)
      par(mfrow = c(2, 3))
      plot(ru1, main = paste('UNIC LCC\n', case$xy, case$rsg, 'kdepaths'))
      plot(ru2, main = paste('UNIC LCC\n',case$xy, case$rsg, 'addedpaths'))
      plot(rc1, main = paste('COLA LCC\n',case$shp, case$tif))
    }
    
    # file.exists(paste0(dataPath, case$lcc_unic_log))
    
    
    ## CRK ----------------
    #size4lcc_sabah_100.addedpaths.txt  size4lcc_sabah_100.kdepaths
    if( !all ( 
      file.exists( 
        paste0(unicorPath, 
               case$crk_unic_out, 
               c('.kdepaths', '.addedpaths.txt') )
      ) 
    ) ) {
      system(case$crk_unic_cmd, intern = TRUE)
    }
    
    if( ! file.exists( paste0(dataPath, case$crk_cola_out) ) ){
      # file.exists(paste0(dataPath, case$crk_unic_log))
      system(case$crk_cola_cmd, intern = TRUE)
    }
    
    if (FALSE){
      ru1 <- raster(paste0(unicorPath, case$crk_unic_out, '.kdepaths')); #plot(ru1)
      ru2 <- raster(paste0(unicorPath, case$crk_unic_out, '.addedpaths.txt')); #plot(ru2)
      rc1 <- raster(paste0(dataPath, case$crk_cola_out)); #plot(rc1)
      par(mfrow = c(1, 3))
      plot(ru1, main = paste('UNIC CRK\n', case$xy, case$rsg, 'kdepaths'))
      plot(ru2, main = paste('UNIC CRK\n',case$xy, case$rsg, 'addedpaths'))
      plot(rc1, main = paste('COLA CRK\n',case$shp, case$tif, ''))
    }
    
    
    ## MAT ----------------
    if( ! file.exists( paste0(unicorPath, case$mat_unic_out, '.cdmatrix.csv') ) ){
      # file.exists(paste0(dataPath, case$crk_unic_log))
      system(case$mat_unic_cmd, intern = TRUE)
    }
    
    
    if( ! file.exists( paste0(dataPath, case$mat_cola_out) ) ){
      system(case$mat_cola_cmd, intern = TRUE)
      # /home/ubuntu/data/out_mat_scenarioT.csv
    }
    
    if(FALSE){
      mu <- read.csv(paste0(unicorPath, case$mat_unic_out, '.cdmatrix.csv'), header = FALSE)
      mc <- read.csv(paste0(dataPath, case$mat_cola_out), header = FALSE);
      dim(mu)
      dim(mc)
      mu[1:5, 1:10]
      mc[1:5, 1:10]
      mu[sort(nrow(mc)-0:5), sort(ncol(mu)-0:5)]
      mc[sort(nrow(mc)-0:5), sort(ncol(mc)-0:5)]
      
    }
    
    sink();sink();sink();sink();
    file.show(sinkLogName)
    
    # read.delim(paste0('/home/ubuntu/data/', case$lcc_unic_log))
    # read.delim(paste0('/home/ubuntu/data/logUNIC_lcc_scenarioX.txt'))
    # read.delim('/home/ubuntu/data/logUNIC_crk_scenarioX.txt')
  }
  i = i +1
}
#write.table(rip_mat, file = 'caseName.rip', col.names = FALSE, row.names = FALSE, quote = FALSE)


outFiles1 <- list.files(path = dataPath, pattern = 'out_.+', full.names = TRUE)
# # sapply(outFiles1, file.remove)
unique(gsub('out_.+_sce', 'sce', gsub('.tif|.csv', '', outFiles1)))

outFiles2 <- list.files(path = unicorPath, pattern = '.cdmatrix.csv|addedpaths|kdepaths')
unique(gsub('out_.+_sce', 'sce', gsub('lcc|crk|mat|.addedpaths|.kdepaths|.cdmatrix|.tif|.csv|.txt', '', outFiles2)))
# # sapply(outFiles2, file.remove)

head(ag[, grep('out', colnames(ag))])

# # # - # system('setsid R -e "source(\'/home/shiny/performance_cola.R\')" >/dev/null 2>&1 < /dev/null &')



bash <- c(ag$lcc_cola_cmd, ag$crk_cola_cmd, ag$mat_cola_cmd,
          ag$lcc_unic_cmd, ag$crk_unic_cmd, ag$mat_unic_cmd)

length(bash)
#repcmd2 <- sample(repcmd2)
#repcmd2 <- gsub('data/log', 'data/log2', repcmd2)
head(repcmd2)

bash <- c('### chmod +x /home/shiny/connectscape/bash_iteration2b.sh    ## Run: /home/shiny/connectscape/bash_iteration2b.sh &', 
             ' ## /home/shiny/connectscape/bash_iteration2b.sh & ', #  > outputfile_for_stdout
             'cd /home/shiny/UNICOR/unicor/',
             '',
             (bash))
bash <- gsub('home/shiny', 'home/ubuntu', bash)


# writeLines(bash, '/home/shiny/connectscape/bash_orig_ubuntu.sh')

# system('chmod +x /home/shiny/connectscape/bash_iteration2b.sh')
# system('/home/shiny/connectscape/bash_iteration2b.sh &')
# 
