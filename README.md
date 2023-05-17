# connectscape
Repo of scripts and tools for landscape connectivity analysis

## Installation steps:
1. Install Docker. Use the stand alone version. Then, create an account with your email. This will install docker platform.
2. Download and double-click on the `installCDPOP.bat` file. Run it as "an administrator" and be sure Docker is running. 
  This will deploy the app your internet explorer direction http://localhost:8501/.  The first time will take some minutes on your PC.
  

## How to use it:
This tool allow you to create connectivity maps for viable animal population.
You need for your analysis:
1. csv file with coordiantes
2. Raster of resistances
3.  ... TBC

![image](https://github.com/gonzalezivan90/connectscape/blob/main/install/steps.png?raw=true)

## Close and relaunch:
The app will be "alive" until you explictly stop the container. You can stop the CONTAINER_NAME (cdpop) by clicking the "stop" buttom on Docker desktop window.
Other option is typing `docker stop cdpop`

For launching the app again, you only need to double click again on the `installCDPOP.bat` file. This will re built the image and launch a new container, however this will take only few seconds. Everytime you double click again on the `installCDPOP.bat` the existing image and container will be deleted, so the computer resources required for running the app are none duplicated.


## Known issues.
If you have problems, please assess the following stages:
- Docker works. Go to the command line (open it by Windows key + R, type `cmd` and hit enter, or find "system symbols" on your windows menu). On the black or blue console, type `docker` and should return the following message:

`Usage:  docker [OPTIONS] COMMAND`
`A self-sufficient runtime for containers`
`Options:`
      `--config string      Location of client config files (default`   
                           `"C:\\Users\\Admin\\.docker") ... `
                              
That means you have docker installed.

- Docker is running: Launch the docker desktop app from your Windows menu or type the following command on the CMD: `"C:\Program Files\Docker\Docker\Docker Desktop.exe"`. Please check a whale little icon next to the time-date bar. You can also double click on the `RunDocker.bat` file

- You have the app folder and files required for the installation downloaded. Open your windows explorer, and paste this path on the direction bar: `%USERPROFILE%`. Check if inside that directory exists the "dockerdata" folder. Inside that folder must be `dockerfile` and `app.py`

