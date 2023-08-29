### chmod +x /home/shiny/connectscape/bash_v2.sh    ## Run: /home/shiny/connectscape/bash_v2.sh &
 ## /home/shiny/connectscape/bash_v2.sh & 
cd /home/shiny/UNICOR/unicor/

### chmod +x /home/shiny/connectscape/bash_v2.sh    ## Run: /home/shiny/connectscape/bash_v2.sh &
 ## /home/shiny/connectscape/bash_v2.sh & 
cd /home/shiny/UNICOR/unicor/

taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/crk.py /home/shiny/data/sabah_500.shp /home/shiny/data/size7.tif /home/shiny/data/out_crk_scenarioAP.tif 1000000 linear 10000 1 &> /home/shiny/data/logCOLA_crk_scenarioAP.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAJ.rip &> /home/shiny/data/logUNIC_crk_scenarioAJ.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAK.rip &> /home/shiny/data/logUNIC_crk_scenarioAK.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAL.rip &> /home/shiny/data/logUNIC_crk_scenarioAL.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAM.rip &> /home/shiny/data/logUNIC_crk_scenarioAM.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAN.rip &> /home/shiny/data/logUNIC_crk_scenarioAN.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAO.rip &> /home/shiny/data/logUNIC_crk_scenarioAO.txt
taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioAP.rip &> /home/shiny/data/logUNIC_crk_scenarioAP.txt
