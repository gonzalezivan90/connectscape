@echo off
title CopyDockerFiles
:: You can copy and paste the commands
:: This commands runs in CMD and not in power Shell. Open your cmd/terminal by
:: WindowsKey+R, then type cmd, then Enter
cd %USERPROFILE%
md dockerdata
cd dockerdata
curl https://raw.githubusercontent.com/gonzalezivan90/connectscape/main/install/dockerfile -O dockerfile
:: Be sure docker is running with the login
docker build -t cdpop_image . 
# docker run -i -t -p 8888:8888 --name cdpop -v %USERPROFILE%\dockerdata:/dockerdata cdpop_image 
docker run -d -i -t -p 8501:8501 --name cdpop -v %USERPROFILE%\dockerdata:/dockerdata cdpop_image 

:: docker exec -it cdpop /bin/bash # Get into the container
pause