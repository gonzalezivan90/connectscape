@echo off
title StopContainerAndRemoveImage
docker stop cdpop
docker rm cdpop
docker rmi cdpop_image