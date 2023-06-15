@echo off
title Installcdujlab

call %USERPROFILE%\Anaconda3\Scripts\activate.bat %USERPROFILE%\Anaconda3
::call C:\ProgramData\Miniconda3\Scripts\activate.bat C:\ProgramData\Miniconda3
call conda activate base
call conda activate cdujlab
call conda info --envs
call pip install pandas ipywidgets ipysheet streamlit

cd %USERPROFILE%
md coladata
cd coladata
curl https://raw.githubusercontent.com/gonzalezivan90/connectscape/main/install/dockerfile -O dockerfile
curl https://raw.githubusercontent.com/rbachati/cdpop_mui/main/test.py -O test.py

cd %USERPROFILE%
cd coladata
call streamlit run %USERPROFILE%/coladata/test.py

:: Change directory to the relative path that's needed for script
rundll32 url.dll,FileProtocolHandler http://localhost:8501/

:: Run script at this location
::call %USERPROFILE%/Anaconda3/envs/%venv%/python.exe "%~dp0\main.py"
PAUSE