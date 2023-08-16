

### chmod +x /home/shiny/connectscape/bashB.sh    ## Run: /home/shiny/connectscape/bashB.sh
# taskset --cpu-list 1 setsid /home/shiny/connectscape/bashB.sh >/dev/null 2>&1 < /dev/null & # NO
# setsid taskset --cpu-list 1 /home/shiny/connectscape/bashB.sh >/dev/null 2>&1 < /dev/null & # NO
# setsid /home/shiny/connectscape/bashB.sh >/dev/null 2>&1 < /dev/null & # NO
# taskset --cpu-list 1 /home/shiny/connectscape/bashB.sh > outputfile_for_stdout & NO
# /home/shiny/connectscape/bashB.sh # yes
# /home/shiny/connectscape/bashB.sh > outputfile_for_stdout & # yes

cd /home/shiny/

rm /home/shiny/Alog*
rm /home/shiny/logUNI*
rm /home/shiny/logCOLA*

cd /home/shiny/UNICOR/unicor/
  
/usr/bin/time -v taskset --cpu-list 1 /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py lcc_scenarioY.rip &> /home/shiny/AlogUNIC_lcc_scenarioY2.txt
taskset --cpu-list 1 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioY.rip &> /home/shiny/AlogUNIC_crk_scenarioY.txt
taskset --cpu-list 1 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py mat_scenarioY.rip &> /home/shiny/AlogUNIC_mat_scenarioY.txt
taskset --cpu-list 1 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python /home/shiny/connecting-landscapes/src/create_cdmat.py /home/shiny/data/sabah_10.shp /home/shiny/data/size5.tif /home/shiny/data/out_mat_scenarioY.csv 100000 1 &> /home/shiny/AlogCOLA_mat_scenarioY.txt


# system("taskset --cpu-list 6 /usr/bin/time -v /home/shiny/anaconda3/envs/cdujlab/bin/python UNICOR.py crk_scenarioH.rip &> /home/shiny/AAlogUNIC_crk_scenarioH.txt")
