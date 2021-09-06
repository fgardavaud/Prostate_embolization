# Prostate_embolization


In our Radiology interventional room, R script to data handling for Limiting radiation exposure during prostatic arteries embolization: influence of patient characteristics, anatomical conditions, and technical factors medical publication

To do that, R script allows to extract exams details from a dose monitoring system. Choose the Prostate_embolization.R version as the v2 is not currently dedicated to this job.
Publication have been published on European Radiology. You can acces throught this [link](https://link.springer.com/article/10.1007/s00330-021-07844-7)

Project lead by Dr. Matthias Barral, CHU Tenon, APHP, France.

# Prerequisite

Severals R packages are mandatory but the script analyze your system and installs, if necessary, missing packages.
In your project root path you have to create one folder named "data" (and put your data here) and another folder named "output" where the output files will be placed there.

# test environment

This script have been tested in R 4.0.4 version in Rstudio v1.4.1106 in macOS Mojave and Catalina version. But it should work on any OS system if your R program have the ability to work with additional R packages

# Additional R packages list available from CRAN

- lubridate
- doMC
- tictoc
- openxlsx
- tidyverse
- rprojroot
