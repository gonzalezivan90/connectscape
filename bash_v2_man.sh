### chmod +x /home/shiny/connectscape/bash_v2.sh    ## Run: /home/shiny/connectscape/bash_v2.sh
## ## /home/shiny/connectscape/bash_v2.sh > outputfile_for_stdout & 
## ## /home/shiny/connectscape/bash_v2.sh & 
cd  /home/shiny/connecting-landscapes/lib/UNICOR/unicor

taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAP.rip &> logUNIC_crk_scenarioAP.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAO.rip &> logUNIC_crk_scenarioAO.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAN.rip &> logUNIC_crk_scenarioAN.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAM.rip &> logUNIC_crk_scenarioAM.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAL.rip &> logUNIC_crk_scenarioAL.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAK.rip &> logUNIC_crk_scenarioAK.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAJ.rip &> logUNIC_crk_scenarioAJ.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif out_mat_scenarioK.csv 100000 1 &> logCOLA_mat_scenarioK.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif out_mat_scenarioJ.csv 100000 1 &> logCOLA_mat_scenarioJ.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif out_mat_scenarioH.csv 100000 1 &> logCOLA_mat_scenarioH.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_500.shp /home/shiny/data/size7.tif out_crk_scenarioAP.tif 1000000 linear 10000 1 &> logCOLA_crk_scenarioAP.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif out_crk_scenarioK.tif 1000000 linear 10000 1 &> logCOLA_crk_scenarioK.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif out_crk_scenarioJ.tif 1000000 linear 10000 1 &> logCOLA_crk_scenarioJ.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif out_crk_scenarioH.tif 1000000 linear 10000 1 &> logCOLA_crk_scenarioH.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif out_lcc_scenarioK.tif 100000 2 100 1 &> logCOLA_lcc_scenarioK.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif out_lcc_scenarioJ.tif 100000 2 100 1 &> logCOLA_lcc_scenarioJ.txt
taskset --cpu-list 2 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif out_lcc_scenarioH.tif 100000 2 100 1 &> logCOLA_lcc_scenarioH.txt
