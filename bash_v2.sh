### chmod +x /home/shiny/connectscape/bash_v2.sh    ## Run: /home/shiny/connectscape/bash_v2.sh
# /home/shiny/connectscape/bash_v2.sh > outputfile_for_stdout & # yes

cd /home/shiny/UNICOR/unicor/

taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif /home/shiny/data/out_lcc_scenarioH.tif 100000 2 100 1 &> /home/shiny/data/logCOLA_lcc_scenarioH.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif /home/shiny/data/out_lcc_scenarioJ.tif 100000 2 100 1 &> /home/shiny/data/logCOLA_lcc_scenarioJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/lcc.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif /home/shiny/data/out_lcc_scenarioK.tif 100000 2 100 1 &> /home/shiny/data/logCOLA_lcc_scenarioK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif /home/shiny/data/out_crk_scenarioH.tif 1000000 linear 10000 1 &> /home/shiny/data/logCOLA_crk_scenarioH.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif /home/shiny/data/out_crk_scenarioJ.tif 1000000 linear 10000 1 &> /home/shiny/data/logCOLA_crk_scenarioJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif /home/shiny/data/out_crk_scenarioK.tif 1000000 linear 10000 1 &> /home/shiny/data/logCOLA_crk_scenarioK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_500.shp /home/shiny/data/size7.tif /home/shiny/data/out_crk_scenarioAP.tif 1000000 linear 10000 1 &> /home/shiny/data/logCOLA_crk_scenarioAP.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_100.shp /home/shiny/data/size2.tif /home/shiny/data/out_mat_scenarioH.csv 100000 1 &> /home/shiny/data/logCOLA_mat_scenarioH.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_200.shp /home/shiny/data/size2.tif /home/shiny/data/out_mat_scenarioJ.csv 100000 1 &> /home/shiny/data/logCOLA_mat_scenarioJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_50.shp /home/shiny/data/size2.tif /home/shiny/data/out_mat_scenarioK.csv 100000 1 &> /home/shiny/data/logCOLA_mat_scenarioK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py lcc_scenarioAN.rip &> /home/shiny/data/logUNIC_lcc_scenarioAN.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py lcc_scenarioAO.rip &> /home/shiny/data/logUNIC_lcc_scenarioAO.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py lcc_scenarioAP.rip &> /home/shiny/data/logUNIC_lcc_scenarioAP.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAH.rip &> /home/shiny/data/logUNIC_crk_scenarioAH.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAI.rip &> /home/shiny/data/logUNIC_crk_scenarioAI.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAJ.rip &> /home/shiny/data/logUNIC_crk_scenarioAJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAK.rip &> /home/shiny/data/logUNIC_crk_scenarioAK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAL.rip &> /home/shiny/data/logUNIC_crk_scenarioAL.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAM.rip &> /home/shiny/data/logUNIC_crk_scenarioAM.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAN.rip &> /home/shiny/data/logUNIC_crk_scenarioAN.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAO.rip &> /home/shiny/data/logUNIC_crk_scenarioAO.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAP.rip &> /home/shiny/data/logUNIC_crk_scenarioAP.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAH.rip &> /home/shiny/data/logUNIC_mat_scenarioAH.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAI.rip &> /home/shiny/data/logUNIC_mat_scenarioAI.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAJ.rip &> /home/shiny/data/logUNIC_mat_scenarioAJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAK.rip &> /home/shiny/data/logUNIC_mat_scenarioAK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAL.rip &> /home/shiny/data/logUNIC_mat_scenarioAL.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAM.rip &> /home/shiny/data/logUNIC_mat_scenarioAM.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAN.rip &> /home/shiny/data/logUNIC_mat_scenarioAN.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAO.rip &> /home/shiny/data/logUNIC_mat_scenarioAO.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioAP.rip &> /home/shiny/data/logUNIC_mat_scenarioAP.txt
