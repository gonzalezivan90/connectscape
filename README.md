# connectscape
Repo of scripts and tools for landscape connectivity analysis

## Installation steps:
1. Install Docker. Use the stand alone version. Then, create an account with your email. This will install docker platform.
2. Download and double-click on the install.bat file. Run it as "an administrator" and be sure Docker is running. 
  This will deploy the app your internet explorer direction http://localhost:8501/. 
  This will create a 

## How to use it:
This tool allow you to create connectivity maps for viable animal population.
You need for your analysis:
1. csv file with coordiantes
2. Raster of resistances
3.  ... TBC


## Known issues.
If you have problems, please assess the following stages:
- Docker works. Go to the command line (open it by Windows key + R, type `cmd` and hit enter, or find "system symbols" on your windows menu). On the black or blue console, type `docker` and should return the following message:

`Usage:  docker [OPTIONS] COMMAND`
`A self-sufficient runtime for containers`
`Options:`
      `--config string      Location of client config files (default`
   `                        "C:\\Users\\Admin\\.docker")`
  `-c, --context string     Name of the context to use to connect to the`
                           `daemon (overrides DOCKER_HOST env var and`
That means you have docker installed.

- Docker is running: Launch the docker desktop app from your Windows menu or type the following command on the CMD: `"C:\Program Files\Docker\Docker\Docker Desktop.exe"`. Please check a whale little icon next to the time-date bar

- You have the app folder and files required for the installation downloaded. Open your windows explorer, and paste this path on the direction bar: `%USERPROFILE%`. Check if inside that directory exists the "dockerdata" folder. Inside that folder must be `dockerfile` and `app.py`

