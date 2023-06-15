@echo off
title CopyDockerFiles
:: You can copy and paste the commands
:: This commands runs in CMD and not in power Shell. Open your cmd/terminal by
:: WindowsKey+R, then type cmd, then Enter
cd %USERPROFILE%
md dockerdata
cd dockerdata
curl https://raw.githubusercontent.com/gonzalezivan90/connectscape/main/install/dockerfile -O dockerfile
curl https://raw.githubusercontent.com/rbachati/cdpop_mui/main/test.py -O test.py



:: Be sure docker is running with the login
::@cmd /k "C:\Program Files\Docker\Docker\Docker Desktop.exe"
::"C:\Program Files\Docker\Docker\Docker Desktop.exe"
docker stop cdpop
docker rm cdpop
docker rmi cdpop_image
docker build -t cdpop_image . 
:: docker run -i -t -p 8888:8888 --name cdpop -v %USERPROFILE%\dockerdata:/dockerdata cdpop_image 
docker run -d -i -t -p 8501:8501 --name cdpop -v %USERPROFILE%\dockerdata:/dockerdata cdpop_image 

:: docker exec -it cdpop /bin/bash # Get into the container
::explorer "http://localhost:8501/"
rundll32 url.dll,FileProtocolHandler http://localhost:8501/
pause