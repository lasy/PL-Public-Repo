
rm(list=ls())
source("Scripts/00_libraries.R")
source("Scripts/00_functions.R")
source("Scripts/00_variables.R")
source("Scripts/00_init.R")

hsmm_sources = paste0("Scripts/my_mhsmm/R/",list.files("Scripts/my_mhsmm/R/"))
for(source in hsmm_sources) source(source)
