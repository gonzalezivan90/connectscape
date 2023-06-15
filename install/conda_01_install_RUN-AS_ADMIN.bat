@echo off
title Installcdujlab

call %USERPROFILE%\Anaconda3\Scripts\activate.bat %USERPROFILE%\Anaconda3
::call C:\ProgramData\Miniconda3\Scripts\activate.bat C:\ProgramData\Miniconda3
call conda activate base
call conda create -n cdujlab networkit networkx h5py pip ipykernel ipywidgets pandas geopandas jupyterlab nb_conda_kernels jupyter_contrib_nbextensions ipysheet -c conda-forge
call conda activate cdujlab
call conda info --envs

:: Change directory to the relative path that's needed for script

:: Run script at this location
::call %USERPROFILE%/Anaconda3/envs/%venv%/python.exe "%~dp0\main.py"
PAUSE